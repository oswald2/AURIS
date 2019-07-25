{-# LANGUAGE
    OverloadedStrings
    , BangPatterns
    , NoImplicitPrelude
#-}
module Conduit.SocketConnector
  ()
where

import           RIO

import           Conduit
import qualified Data.Streaming.Network as SN
import           UnliftIO.Exception


import           Control.PUS.Classes




runGeneralTCPReconnectClient
  :: MonadUnliftIO m
  => SN.ClientSettings
  -> (SN.AppData -> m a)
  -> m a
runGeneralTCPReconnectClient set f = withRunInIO $ \run ->
    runTCPClient set $ run . f


runTCPReconnectClient :: SN.ClientSettings -> (SN.AppData -> IO a) -> IO a
runTCPReconnectClient (SN.ClientSettings port host addrFamily readBufferSize) app = E.bracket
    (getSocketFamilyTCP host port addrFamily)
    (NS.close . fst)
    (\(s, address) -> app AppData
        { appRead' = safeRecv s readBufferSize
        , appWrite' = sendAll s
        , appSockAddr' = address
        , appLocalAddr' = Nothing
        , appCloseConnection' = NS.close s
        , appRawSocket' = Just s
        })

    
-- reconnectConduit
--   :: (MonadIO m, MonadUnliftIO m, MonadReader env m, HasGlobalState env)
--   => Text
--   -> Word16
--   -> Int
--   -> (ConduitT i ByteString m () -> m a)
--   -> m a 
-- reconnectConduit host port' delay = do
--   term <- catch
--     (worker)
--     (\e -> do
--       logWarn $ "SocketConnector: " <> displayShow (e :: SomeException)
--       return False
--     )
--   unless term $ do
--     logInfo $ "Could not connect to: " <> display host <> " retrying..."
--     threadDelay delay
--     reconnectConduit host port' delay

--  where
--   worker = do
--     let port = fromIntegral port'

--     bracket
--       (socket AF_INET Stream defaultProtocol)
--       (\sock -> do
--         close sock
--         ifRaiseEvent interface discEvent
--       )
--       (\sock -> do
--         setSocketOption sock ReuseAddr 1
--         he <- getHostByName (config ^. cfgHost)
--         connect sock (SockAddrInet port (hostAddress he))

--         ifRaiseEvent interface (connEvent)

--         handler sock interface
--         return False
--       )








-- socketConnector :: Word16
--                 -> Config
--                 -> Interface
--                 -> (Socket -> Interface -> IO ())
--                 -> (Event, Event)
--                 -> IO ()
-- socketConnector port' config interface handler ev@(connEvent, discEvent) = do
--     term <- catch (worker port' config interface handler ev) $ (\e -> do
--         logStr lAreaAlways $ "socketConnector: Exception: " ++ show (e :: SomeException)
--         return False
--         )
--     when (not term) $ do
--         logStr lAreaAlways $ "Could not connect to: " ++ (config^.cfgHost) ++ " retrying..."
--         threadDelay 5000000
--         socketConnector port' config interface handler (connEvent, discEvent)

--     where
--         worker port' config interface handler (connEvent, discEvent) = do
--             let port = fromIntegral port'

--             bracket
--                 (socket AF_INET Stream defaultProtocol)
--                 (\sock -> do
--                     close sock
--                     ifRaiseEvent interface discEvent
--                 )
--                 (\sock -> do
--                     setSocketOption sock ReuseAddr 1
--                     he <- getHostByName  (config^.cfgHost)
--                     connect sock (SockAddrInet port (hostAddress he))

--                     ifRaiseEvent interface (connEvent)

--                     handler sock interface
--                     return False
--                 )
