module Protocol.NDIULiteProcessor
    ( ndiuSenderChainC
    , runNdiuChain
    , createNdiuC
    , ndiuToTMFrameC
    ) where

import           RIO
import qualified RIO.Text                      as T

import           Conduit
import           Conduit.Extras
import           Conduit.SocketConnector

import           Control.Concurrent.Killable
import           Control.PUS.Classes

import           Data.Attoparsec.ByteString    as A
import           Data.Conduit.Network

import           Protocol.NDIULite

import           Data.PUS.Config
import           Data.PUS.Events
import           Data.PUS.TCFrameTypes
import           Data.PUS.TMFrame
import           Data.PUS.TMFrameExtractor
import           Data.PUS.TMStoreFrame

import           General.Time
import           General.Types

import           System.Timer.Updatable

import           Protocol.ProtocolInterfaces
import           Protocol.ProtocolSwitcher

import           Data.Time.Clock.System


ndiuSenderChainC
    :: (MonadIO m, MonadReader env m, HasLogFunc env)
    => NDIULiteConfig
    -> TBQueue NdiuCmd
    -> ConduitT () ByteString m ()
ndiuSenderChainC cfg queue = do
    let chain = if cfgNdiuHeartbeatEnable cfg then ndiuHeartbeatC sendInterval else ndiuC
        sendInterval = fromIntegral (cfgNdiuHeartbeatSendTime cfg) * 1_000_000

    chain .| ndiuEncodeC
  where
    ndiuC = do
        res <- atomically $ readTBQueue queue
        case res of
            NdiuMsg ndiu -> do
                yield ndiu
                ndiuC
            NdiuQuit -> return ()

    ndiuHeartbeatC sendInterval = do
        res <- readWithTimeout sendInterval queue
        case res of
            Nothing -> do
                heartbeat <- createNdiuHeartbeatMsg
                yield heartbeat
                logDebug "Sent NDIU Hearbeat"
                ndiuHeartbeatC sendInterval 
            Just (NdiuMsg ndiu) -> do
                yield ndiu
                ndiuHeartbeatC sendInterval
            Just NdiuQuit -> return ()



runNdiuChain
    :: (MonadUnliftIO m, MonadReader env m, HasLogFunc env, HasRaiseEvent env)
    => TMFrameConfig
    -> NDIULiteConfig
    -> SwitcherMap
    -> TBQueue NdiuCmd
    -> m ()
runNdiuChain tmFrameCfg cfg vcMap queue = do
    var <- newTVarIO Nothing
    runGeneralTCPReconnectClient
        (clientSettings (fromIntegral (cfgNdiuPort cfg))
                        (encodeUtf8 (cfgNdiuHost cfg))
        )
        200_000
        (processConnect tmFrameCfg cfg vcMap queue var)
    onDisconnect cfg var


processConnect
    :: (MonadUnliftIO m, MonadReader env m, HasLogFunc env, HasRaiseEvent env)
    => TMFrameConfig
    -> NDIULiteConfig
    -> SwitcherMap
    -> TBQueue NdiuCmd
    -> TVar (Maybe (Updatable ()))
    -> AppData
    -> m ()
processConnect tmFrameConfig cfg vcMap queue var appData = do
    raiseEvent
        (EVAlarms (EVEConnection (IfNdiu (cfgNdiuID cfg)) ConnSingle Connected))
    logInfo $ "Connected on NDIU " <> display (cfgNdiuID cfg)
    startTimers cfg queue var
    race_ readThread writeThread
  where
    readThread = do
        logDebug "NDIU read thread enters"
        runConduitRes
            $  appSource appData
            .| ndiuDecodeC
            .| processNdiu tmFrameConfig cfg vcMap var

        logDebug "NDIU read thread leaves"

    writeThread = do
        logDebug "NDIU write thread enters"
        runConduitRes (ndiuSenderChainC cfg queue .| appSink appData)
        logDebug "NDIU write thread leaves"


processNdiu
    :: (MonadUnliftIO m, MonadReader env m, HasLogFunc env, HasRaiseEvent env)
    => TMFrameConfig
    -> NDIULiteConfig
    -> SwitcherMap
    -> TVar (Maybe (Updatable ()))
    -> ConduitT NDIU Void m ()
processNdiu tmFrameCfg cfg vcMap var =
    src .| ndiuToTMFrameC tmFrameCfg cfg .| tmFrameSwitchVC vcMap

  where
    src = do
        awaitForever $ \ndiu -> do
            lift $ restartTimer cfg var
            case ndiuType ndiu of
                NdiuHeartBeat -> return () 
                NdiuTmGood    -> yield ndiu 
                NdiuTmBad     -> yield ndiu 
                NdiuTc        -> yield ndiu
                NdiuUnknown t -> do
                    logWarn
                        $  "Unknown NDIU Lite message received (type "
                        <> display t
                        <> "): "
                        <> display ndiu


onDisconnect
    :: (MonadIO m, MonadReader env m, HasLogFunc env, HasRaiseEvent env)
    => NDIULiteConfig
    -> TVar (Maybe (Updatable ()))
    -> m ()
onDisconnect cfg var = do
    action <- atomically $ do
        hbTimer <- readTVar var
        return $ forM_ hbTimer kill
    liftIO $ action
    raiseEvent
        (EVAlarms
            (EVEConnection (IfNdiu (cfgNdiuID cfg)) ConnSingle Disconnected)
        )
    logWarn $ "Disconnected on NDIU " <> display (cfgNdiuID cfg)


startTimers
    :: (MonadUnliftIO m, MonadReader env m, HasLogFunc env)
    => NDIULiteConfig
    -> TBQueue NdiuCmd
    -> TVar (Maybe (Updatable ()))
    -> m ()
startTimers cfg queue var = do
    when
            (  cfgNdiuHeartbeatEnable cfg
            && (  (cfgNdiuDirection cfg == DirectionIn)
               || (cfgNdiuDirection cfg == DirectionIO)
               )
            )
        $ do
              let time = fromIntegral (cfgNdiuHeartbeatTimeout cfg) * 1_000_000
              env     <- ask
              hbTimer <- liftIO $ replacer
                  (runRIO env (heartBeatReceiveTimeOut queue))
                  time

              atomically $ writeTVar (var) (Just hbTimer)


heartBeatReceiveTimeOut
    :: (MonadUnliftIO m, MonadReader env m, HasLogFunc env)
    => TBQueue NdiuCmd
    -> m ()
heartBeatReceiveTimeOut queue = do
    logWarn "NDIULite heartbeat timeout!"
    atomically $ writeTBQueue queue NdiuQuit


restartTimer
    :: (MonadUnliftIO m)
    => NDIULiteConfig
    -> TVar (Maybe (Updatable ()))
    -> m ()
restartTimer cfg var = do
    let hbTime = fromIntegral (cfgNdiuHeartbeatTimeout cfg) * 1_000_000
    -- logDebug "Restarting HeartBeat Reception Timer"
    atomically $ do
        timer <- readTVar var
        case timer of
            Nothing -> return ()
            Just t  -> renew t hbTime



createNdiuC :: (MonadIO m) => ConduitT QueueMsg NdiuCmd m ()
createNdiuC = do
    awaitForever $ \case
        EQPacket _     -> return ()
        EQCLTU   _     -> return ()
        EQFrame  frame -> do
            res <- frameToNDIU frame
            yield (NdiuMsg res)


frameToNDIU :: (MonadIO m) => EncodedTCFrame -> m NDIU
frameToNDIU encFrame = do
    createNdiuMessage NdiuTmGood (encFrame ^. encTcFrameData)


ndiuToTMFrameC
    :: (MonadIO m, MonadReader env m, HasLogFunc env, HasRaiseEvent env)
    => TMFrameConfig
    -> NDIULiteConfig
    -> ConduitT NDIU TMStoreFrame m ()
ndiuToTMFrameC tmFrameCfg cfg = awaitForever $ \ndiu -> do
    case A.parseOnly (tmFrameParser tmFrameCfg) (toBS (ndiuData ndiu)) of
        Left err -> do
            let msg = T.pack err
            lift
                $ raiseEvent
                      (EVAlarms
                          (EVIllegalTMFrame
                              ("Could not parse TM Frame: " <> msg)
                          )
                      )
            logWarn $ "Could not parse TM Frame: " <> display msg
            logDebug $ "TM Frame:\n" <> display (ndiuData ndiu)
        Right frame -> do
            logDebug
                $  display ("Received TM Frame: " :: Text)
                <> displayShow frame

            let ert = fromUTC (systemToUTCTime t)
                t = MkSystemTime (fromIntegral (ndiuSecs ndiu)) (ndiuNano ndiu)

                storeFrame = TMStoreFrame
                    { _tmstTime      = ert
                    , _tmstInterface = IfNdiu (cfgNdiuID cfg)
                    , _tmstFrame     = frame
                    , _tmstBinary    = ndiuData ndiu
                    }

            yield storeFrame
