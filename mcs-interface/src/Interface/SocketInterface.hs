{-# LANGUAGE OverloadedStrings
    , BangPatterns
    , NoImplicitPrelude
    , LambdaCase
#-}
module Interface.SocketInterface
    ( InterfaceType(..)
    , initSocketInterface
    , Server
    , hasConnection
    )
where


import           RIO
import qualified RIO.List.Partial              as L
import qualified Data.ByteString.Char8         as BC
import qualified Data.ByteString.Lazy          as BL
import qualified RIO.Text                      as T

import           Text.Read

import           Control.Concurrent.STM.TMChan

import           Conduit
import qualified Data.Conduit.Combinators      as C
import           Data.Conduit.Binary           as CB
import           Data.Conduit.Network
import           Data.Conduit.TMChan
import           Data.Word8
import           Data.Aeson                     ( eitherDecode
                                                , encode
                                                )
import           Codec.Serialise

import           Data.PUS.Config
import           Data.PUS.Events

import           Interface.Interface
import           Interface.Actions
import           Interface.Executor
import           Interface.Events


data QuitException = QuitException
    deriving (Read, Show)

instance Exception QuitException where

-- | Determines the type of the message exchange on the interface
-- Can be either via Haskell Strings (Read, Show instances) when
-- 'InterfaceString', via Binary generated structures when 'InterfaceBinary'
-- and via JSON (line based) when 'InterfaceJSON'
data InterfaceType =
    InterfaceString
    | InterfaceSerialise
    | InterfaceJSON
    deriving (Eq, Ord, Enum, Show, Read)


-- | The internal server type
newtype Server = Server {
    eventChan :: TVar (Maybe (TMChan IfEvent))
}

-- | Returns true if there is currently a connection
hasConnection :: (MonadIO m) => Server -> m Bool
hasConnection server = isJust <$> readTVarIO (eventChan server)


-- | Initializes the socket interface, starts the necessary threads
-- and listens on the socket for incoming connections
-- Returns an 'Interface' which should be added to the available
-- interfaces
initSocketInterface
    :: (MonadIO m, MonadUnliftIO m, MonadReader env m, HasLogFunc env)
    => Config
    -> InterfaceType
    -> Interface
    -> m Interface
initSocketInterface config ifType interface = case cfgInterfacePort config of
    Nothing   -> return interface
    Just port -> do
        var <- newTVarIO Nothing

        let newInterface = addInterface interface (localRaiseEvent server)
            server       = Server { eventChan = var }

        void $ async $ initServer ifType port server newInterface

        return newInterface



localRaiseEvent :: (MonadIO m) => Server -> IfEvent -> m ()
localRaiseEvent server ev = atomically $ do
    res <- readTVar (eventChan server)
    case res of
        Just chan -> writeTMChan chan ev
        Nothing   -> return ()



initServer
    :: (MonadIO m, MonadUnliftIO m, MonadReader env m, HasLogFunc env)
    => InterfaceType
    -> Word16
    -> Server
    -> Interface
    -> m ()
initServer ifType port server interface =
    runGeneralTCPServer (serverSettings (fromIntegral port) "*") $ \app -> do
        chan <- liftIO newTMChanIO
        atomically $ writeTVar (eventChan server) (Just chan)
        race_
            (runConduit
                (  appSource app
                .| readConduit ifType interface
                .| actionSink interface
                )
            )
            (runConduit (sourceTMChan chan .| showConduit ifType .| appSink app)
            )
        -- on closing the connection, set the Chan back to Nothing
        atomically $ writeTVar (eventChan server) Nothing


parseAction
    :: (Monad m)
    => InterfaceType
    -> ConduitT ByteString (Either Text Action) m ()
parseAction InterfaceString = C.filterE (/= _cr) .| CB.lines .| proc
  where
    proc = awaitForever $ \bs -> do
        let res = reads (BC.unpack bs)
        if null res
            then do
                let msg = case decodeUtf8' bs of
                        Left  err -> T.pack (show err)
                        Right m   -> m
                yield (Left msg)
            else yield (Right (fst (L.head res)))
parseAction InterfaceSerialise = awaitForever $ \bs -> do
    case deserialiseOrFail (BL.fromStrict bs) of
        Left  (DeserialiseFailure _ err) -> yield (Left (T.pack err))
        Right x                          -> yield (Right x)
parseAction InterfaceJSON = C.filterE (/= _cr) .| CB.lines .| proc
  where
    proc = awaitForever $ \bs -> do
        case eitherDecode (BL.fromStrict bs) of
            Left  err -> yield (Left (T.pack err))
            Right a   -> yield (Right a)


readConduit
    :: (MonadIO m, MonadUnliftIO m, MonadReader env m, HasLogFunc env)
    => InterfaceType
    -> Interface
    -> ConduitT ByteString Action m ()
readConduit ifType interface = parseAction ifType .| proc
  where
    proc = awaitForever $ \case
        Left err -> liftIO $ ifRaiseEvent
            interface
            (EventPUS (EVAlarms (EVIllegalAction err)))
        Right action' -> case action' of
            ActionQuit -> do
                logWarn
                    "Received QUIT from Socket Interface, shutting down socket connection..."
                throwIO QuitException
            action -> yield action



actionSink :: (MonadIO m) => Interface -> ConduitT Action Void m ()
actionSink interface =
    awaitForever $ \action -> liftIO $ executeAction interface action



showConduit :: (Monad m) => InterfaceType -> ConduitT IfEvent ByteString m ()
showConduit InterfaceString = awaitForever $ \ev -> do
    yield (BC.pack (show ev ++ "\n"))
showConduit InterfaceSerialise = awaitForever $ \ev -> do
    yield (BL.toStrict (serialise ev))
showConduit InterfaceJSON = awaitForever $ \ev -> do
    yield (BL.toStrict (Data.Aeson.encode ev) `BC.snoc` '\n')
