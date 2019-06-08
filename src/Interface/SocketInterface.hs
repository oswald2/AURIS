{-# LANGUAGE OverloadedStrings
    , BangPatterns
    , NoImplicitPrelude

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
import           Data.Binary                    ( decodeOrFail )
import           Data.Aeson                     ( eitherDecode )

import           Data.PUS.Events
import           Data.PUS.Config

import           Interface.Interface
import           Interface.Actions
import           Interface.Executor


data QuitException = QuitException
    deriving (Read, Show)

instance Exception QuitException where


data InterfaceType =
    InterfaceString
    | InterfaceBinary
    | InterfaceJSON
    deriving (Eq, Ord, Enum, Show, Read)


data Server = Server {
    eventChan :: TVar (Maybe (TMChan Event))
}


hasConnection :: (MonadIO m) => Server -> m Bool
hasConnection server = do
    res <- readTVarIO (eventChan server)
    return $ isJust res



initSocketInterface
    :: ( MonadIO m
       , MonadUnliftIO m
       , MonadReader env m
       , HasLogFunc env
       )
    => Config
    -> InterfaceType
    -> Interface
    -> m Interface
initSocketInterface config ifType interface = do
    case cfgInterfacePort config of
        Nothing   -> return interface
        Just port -> do
            var <- newTVarIO Nothing

            let newInterface = addInterface interface (localRaiseEvent server)
                server       = Server { eventChan = var }

            void $ async $ initServer ifType port server newInterface

            return newInterface



localRaiseEvent :: (MonadIO m) => Server -> Event -> m ()
localRaiseEvent server ev = do
    atomically $ do
        res <- readTVar (eventChan server)
        case res of
            Just chan -> writeTMChan chan ev
            Nothing   -> return ()



initServer
    :: ( MonadIO m
       , MonadUnliftIO m
       , MonadReader env m
       , HasLogFunc env
       )
    => InterfaceType
    -> Word16
    -> Server
    -> Interface
    -> m ()
initServer ifType port server interface = do
    runGeneralTCPServer (serverSettings (fromIntegral port) "*") $ \app -> do
        chan <- liftIO $ newTMChanIO
        atomically $ writeTVar (eventChan server) (Just chan)
        race_
            (runConduit
                (  appSource app
                .| C.filterE (/= _cr)
                .| CB.lines
                .| readConduit ifType interface
                .| actionSink interface
                )
            )
            (runConduit (sourceTMChan chan .| showConduit .| appSink app))
        -- on closing the connection, set the Chan back to Nothing
        atomically $ writeTVar (eventChan server) Nothing


parseAction :: InterfaceType -> ByteString -> Either Text Action
parseAction InterfaceString bs =
    let res = reads (BC.unpack bs)
    in  if null res
            then
                let msg = case decodeUtf8' bs of
                        Left  err -> T.pack (show err)
                        Right m   -> m
                in  Left msg
            else Right $ fst (L.head res)
parseAction InterfaceBinary bs = case decodeOrFail (BL.fromStrict bs) of
    Left  (_, _, err) -> Left (T.pack err)
    Right (_, _, x  ) -> Right x
parseAction InterfaceJSON bs = case eitherDecode (BL.fromStrict bs) of
    Left  err -> Left (T.pack err)
    Right a   -> Right a


readConduit
    :: ( MonadIO m
       , MonadUnliftIO m
       , MonadReader env m
       , HasLogFunc env
       )
    => InterfaceType
    -> Interface
    -> ConduitT ByteString Action m ()
readConduit ifType interface = awaitForever $ \bs -> do
    case parseAction ifType bs of
        Left err ->
            liftIO $ ifRaiseEvent interface (EVAlarms (EV_IllegalAction err))
        Right action' -> case action' of
            ActionQuit -> do
                logWarn
                    "Received QUIT from Socket Interface, shutting down socket connection..."
                throwIO QuitException
            action -> executeAction interface action



actionSink :: (MonadIO m) => Interface -> ConduitT Action Void m ()
actionSink interface =
    awaitForever $ \action -> liftIO $ executeAction interface action



showConduit :: (Monad m) => ConduitT Event ByteString m ()
showConduit = awaitForever $ \ev -> yield (BC.pack ((show ev) ++ "\n"))
