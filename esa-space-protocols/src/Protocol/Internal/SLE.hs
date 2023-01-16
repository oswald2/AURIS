module Protocol.Internal.SLE
    ( startSLE
    ) where

import           Control.PUS.Classes
import           RIO
import qualified RIO.HashMap                   as HM
import qualified RIO.Text                      as T

import           Data.PUS.Events

import           SLE.Interface
import           SLE.Log
import           SLE.Types

import           Text.Builder                   ( run )

import           Data.PUS.Config
import           Data.PUS.EncTime
import           Data.PUS.TMFrame
import           Data.PUS.TMFrameExtractor
import           Data.PUS.TMStoreFrame

import           General.Hexdump
import           General.Time
import           General.Types

import           Protocol.Internal.FCLTU
import           Protocol.Internal.RAF
import           Protocol.Internal.SLETypes
import           Protocol.ProtocolInterfaces
import           Protocol.ProtocolSLE

import           Text.Show.Pretty


startSLE
    :: ( MonadUnliftIO m
       , MonadReader env m
       , HasLogFunc env
       , HasConfig env
       , HasRaiseEvent env
       )
    => SLEConfig
    -> SwitcherMap
    -> TBQueue SLECommand
    -> m ()
startSLE sleCfg vcMap cmdQueue = do
    state   <- ask
    queues' <-
        traverse
                (createQueue . SleSII . sleInstanceCfgSII . cfgSleInstanceConfig
                )
            $ cfgSleInstances sleCfg

    let cbs    = callbacks state vcMap queues
        queues = HM.fromList queues'

    withSLEUser
        (SeConfigFile (cfgSleSeConfig sleCfg))
        (ProxyConfigFile (cfgSleProxyConfig sleCfg))
        cbs
        (sleUser queues' queues)
  where
    sleUser queues' queues sle = do 
        liftIO $ interfaceTraceLevel sle SleTraceFull
        race_ (processing sleCfg queues' sle) (commandThread queues)


    createQueue sii = do
        q <- liftIO $ newTBQueueIO 100
        pure (sii, q)

    commandThread queues = do
        let sendTo sii cmd = do
                case HM.lookup (SleSII sii) queues of
                    Just q -> do
                        atomically $ writeTBQueue q cmd
                    Nothing -> pure ()


        cmd <- atomically $ readTBQueue cmdQueue
        case cmd of
            SLETerminate -> atomically $ do
                mapM_ (\q -> writeTBQueue (snd q) Terminate) (HM.toList queues)
                -- we loop over, as we use race_ above and wait for the 
                -- interfaces to terminate. Our thread will be termianted
                -- automatically when all others are shutdown
            SLEBindRaf     sii -> sendTo sii RafBind
            SLEUnbindRaf   sii -> sendTo sii RafUnbind
            SLEStartRaf    sii -> sendTo sii RafStart
            SLEStopRaf     sii -> sendTo sii RafStop

            SLEBindFcltu   sii -> sendTo sii FcltuBind
            SLEUnbindFcltu sii -> sendTo sii FcltuUnbind
            SLEStartFcltu  sii -> sendTo sii FcltuStart
            SLEStopFcltu   sii -> sendTo sii FcltuStop


        commandThread queues


processing
    :: (MonadUnliftIO m, MonadReader env m, HasLogFunc env, HasRaiseEvent env)
    => SLEConfig
    -> [(SleSII, TBQueue SleCmd)]
    -> SLE
    -> m ()
processing sleCfg queues sle = do
    let instances = zipWith inst (cfgSleInstances sleCfg) queues
        inst i q = conc (startInstance sle (cfgSlePeerID sleCfg) i q)
        threads = foldr (<>) mempty instances
    runConc threads


startInstance
    :: (MonadUnliftIO m, MonadReader env m, HasLogFunc env, HasRaiseEvent env)
    => SLE
    -> Text
    -> SLEInstance
    -> (SleSII, TBQueue SleCmd)
    -> m ()
startInstance sle peerID (SLEInstance { cfgSleInstanceNr = rafN, cfgSleInstanceConfig = SLEInstRAF rafCfg }) (_sii, queue)
    = do
        let version = convVersion (cfgSleRafVersion rafCfg)
            sii     = SleSII (cfgSleRafSII rafCfg)

        raiseEvent
            (EVSLE
                (EVSLEInitRaf (IfSle (SleRAFIf rafN))
                              sii
                              version
                              peerID
                              (cfgSleRafPort rafCfg)
                )
            )

        logDebug $ "Starting RAF instance for " <> display (cfgSleRafSII rafCfg)
        res <- withSleRAFUser sle
                              (cfgSleRafSII rafCfg)
                              version
                              (cfgSleRafPeerID rafCfg)
                              (cfgSleRafPort rafCfg)
                              Nothing
                              Nothing
                              (runRAF peerID rafCfg sii queue)
        forM_ res $ \err ->
            logError
                $  "SLE Instance "
                <> display (cfgSleRafSII rafCfg)
                <> " returned: "
                <> display err
        pure ()

startInstance sle peerID (SLEInstance { cfgSleInstanceNr = _rafN, cfgSleInstanceConfig = SLEInstFCLTU cltuCfg }) (_sii, queue)
    = do
        let version = convVersion (cfgSleCltuVersion cltuCfg)
            sii     = SleSII (cfgSleCltuSII cltuCfg)

        raiseEvent
            (EVSLE (EVSLEInitFcltu sii version peerID (cfgSleCltuPort cltuCfg)))

        logDebug $ "Starting FCLTU instance for " <> display
            (cfgSleCltuSII cltuCfg)
        res <- withSleFCLTUUser sle
                                (SleSII (cfgSleCltuSII cltuCfg))
                                (convVersion (cfgSleCltuVersion cltuCfg))
                                (cfgSleCltuPeerID cltuCfg)
                                (cfgSleCltuPort cltuCfg)
                                Nothing
                                Nothing
                                (runFCLTU peerID cltuCfg sii queue)
        forM_ res $ \err ->
            logError
                $  "SLE Instance "
                <> display (cfgSleCltuSII cltuCfg)
                <> " returned: "
                <> display err
        pure ()

startInstance _ _ _ _ = pure ()




-- convDeliveryMode :: SLEDeliveryMode -> SleDeliveryMode
-- convDeliveryMode SLEOnlineComplete = SleCompleteOnline
-- convDeliveryMode SLEOnlineTimely   = SleTimelyOnline
-- convDeliveryMode SLEOffline        = SleOffline



callbacks
    :: (HasLogFunc env, HasConfig env, HasRaiseEvent env)
    => env
    -> SwitcherMap
    -> HashMap SleSII (TBQueue SleCmd)
    -> Callbacks
callbacks state vcMap cmdQueues = Callbacks
    { cbLogHandler                 = sleLog state
    , cbNotifyHandler              = sleNotify state
    , cbTraceHandler               = tracer state
    , cbUnexpectedHandler          = unexpectedCB state
    , cbAsyncNotifyHandler         = asyncCB state
    , cbPeerAbortHandler           = peerAbortCB state cmdQueues
    , cbTransferBufferHandler      = transferBufCB state
    , cbStatusReportHandler        = statusReportCB state
    , cbSyncNotifyHandler          = syncCB state
    , cbTransferDataHandler        = transferDataCB state vcMap
    , cbOpReturnHandler            = opReturnCB state cmdQueues
    , cbResumeDataTransferHandler  = resumeDataTransferCB state
    , cbProvisionPeriodEndsHandler = provisionEndsCB state
    , cbProtocolAbortHandler       = protocolAbortCB state
    , cbBindHandler                = bindCB state
    , cbUnbindHandler              = unbindCB state
    , cbCLTUStartHandler           = cltuStartCB state
    , cbCLTUTransferDataHandler    = cltuTransDataCB state
    , cbCLTUNegTransferHandler     = cltuNegTransCB state
    , cbCLTUThrowEventHandler      = throwCB state
    , cbStopHandler                = stopCB state
    , cbRAFStartHandler            = rafStartCB state
    , cbRCFStartHandler            = rcfStartCB state
    }



sleLog :: (HasLogFunc env) => env -> SleLogHandler
sleLog state SleLogMsgAlarm msg = runRIO state $ do
    logError $ "SLE ALARM: " <> display (run msg)
sleLog state SleLogMsgInfo msg = runRIO state $ do
    logInfo $ "SLE INFO: " <> display (run msg)

sleNotify :: (HasLogFunc env) => env -> SleNotifyHandler
sleNotify state msg = runRIO state $ do
    logDebug $ "SLE NOTIFY: " <> display (run msg)


tracer :: (HasLogFunc env) => env -> SleTraceHandler
tracer state msg = runRIO state $ do
    logDebug $ "SLE TRACE: " <> display (run msg)

unexpectedCB :: (HasLogFunc env) => env -> SleUnexpectedHandler
unexpectedCB state msg = runRIO state $ do
    logWarn $ "SLE UNEXPECTED: " <> display (run msg)

asyncCB :: (HasLogFunc env) => env -> SleAsyncNotifyHandler
asyncCB state msg = runRIO state $ do
    logDebug $ "SLE ASYNC: " <> display (run msg)

peerAbortCB
    :: (HasLogFunc env)
    => env
    -> HashMap SleSII (TBQueue SleCmd)
    -> SlePeerAbortHandler
peerAbortCB state siiMap sii diag originator = runRIO state $ do
    logWarn
        $  "SLE PEER ABORT: "
        <> display sii
        <> " diagnostic: "
        <> display diag
        <> " originator: "
        <> display originator
    case HM.lookup sii siiMap of
        Nothing -> pure ()
        Just q  -> atomically $ writeTBQueue q (PeerAbort sii diag originator)

transferBufCB :: env -> SleTransferBufferHandler
transferBufCB state _count = runRIO state $ pure ()

statusReportCB :: (HasLogFunc env) => env -> SleStatusReportHandler
statusReportCB state linkType msg = runRIO state $ do
    logInfo
        $  "SLE STATUS REPORT: Link Type: "
        <> display linkType
        <> ": "
        <> display (run msg)

syncCB :: (HasLogFunc env) => env -> SleSyncNotifyHandler
syncCB state _linkType msg = runRIO state $ do
    logDebug $ "SLE ASYNC: " <> display (run msg)

transferDataCB
    :: (HasLogFunc env, HasRaiseEvent env, HasConfig env)
    => env
    -> SwitcherMap
    -> SleTransferDataHandler
transferDataCB state vcMap linkType seqCnt ert cont frame = runRIO state $ do
    logDebug
        $  "SLE TRANSFER DATA: "
        <> display linkType
        <> " SeqCount: "
        <> display seqCnt
        <> " ERT: "
        <> displayShow ert
        <> " Cont: "
        <> display cont
        <> " Frame: "
        <> fromString (ppShow frame)
    case decodeFrame (cfgTMFrame (state ^. getConfig)) frame of
        Left err -> do
            logError
                $  "Error decoding frame: "
                <> display (T.pack err)
                <> ": "
                <> display (HexBytes frame)
        Right tmFrame -> do
            let interf = case linkType of
                    SleRAF       -> IfSle (SleRAFIf undefined)
                    SleRCF       -> IfSle (SleRCFIf undefined)
                    SleCLTU      -> IfSle (SleFCLTUIf undefined)
                    SleUnknown _ -> IfSle SleUnknownIf

            case decodeCdsTime ert of
                Left err ->
                    logError
                        $  "Error decoding Frame ERT: "
                        <> display err
                        <> display @Text ": "
                        <> display (hexdumpBS ert)
                Right decodedERT -> do
                    let storeFrame = TMStoreFrame
                            { _tmstFrame     = tmFrame
                            , _tmstBinary    = HexBytes frame
                            , _tmstInterface = interf
                            , _tmstTime      = timeStamp
                            }
                        ep        = epoch1958 (LeapSeconds 0)
                        timeStamp = cdsTimeToSunTime ep decodedERT
                    multiplexFrame vcMap storeFrame

opReturnCB
    :: (HasLogFunc env)
    => env
    -> HashMap SleSII (TBQueue SleCmd)
    -> SleOpReturnHandler
opReturnCB state hm sii seqCnt opType appID result invokeID dat =
    runRIO state $ do
        logDebug
            $  "SLE OP RETURN: "
            <> display (run (sleSIIBuilder sii))
            <> " SeqCount: "
            <> display seqCnt
            <> " OP: "
            <> display opType
            <> " AppID: "
            <> display appID
            <> " Result: "
            <> display result
            <> " InvokeID: "
            <> display invokeID
            <> " Data: "
            <> display dat
        case opType of
            SleOpBind -> case appID of
                SleRtnAllFrames -> case result of
                    SleResultPositive -> sendToSii sii (RafBindSuccess sii)
                    SleResultNegative -> sendToSii sii (RafBindError sii dat)
                    _                 -> pure ()
                SleFwdCltu -> case result of
                    SleResultPositive -> sendToSii sii (FcltuBindSuccess sii)
                    SleResultNegative -> sendToSii sii (FcltuBindError sii dat)
                    _                 -> pure ()
                _ -> pure ()
            SleOpStart -> case appID of
                SleRtnAllFrames -> case result of
                    SleResultPositive -> sendToSii sii (RafStartSuccess sii)
                    SleResultNegative -> sendToSii sii (RafStartError sii dat)
                    _                 -> pure ()
                SleFwdCltu -> case result of
                    SleResultPositive -> sendToSii sii (FcltuStartSuccess sii)
                    SleResultNegative ->
                        sendToSii sii (FcltuStartError sii dat)
                    _ -> pure ()
                _ -> pure ()
            _ -> pure ()
  where
    sendToSii sii2 cmd = do
        case HM.lookup sii2 hm of
            Just queue -> atomically $ writeTBQueue queue cmd
            Nothing    -> pure ()

resumeDataTransferCB :: (HasLogFunc env) => env -> SleResumeDataTransferHandler
resumeDataTransferCB state sii = runRIO state $ do
    logDebug $ "SLE RESUME TRANSFER: " <> display (run (sleSIIBuilder sii))


provisionEndsCB :: (HasLogFunc env) => env -> SleProvisionPeriodEndsHandler
provisionEndsCB state sii = runRIO state $ do
    logWarn $ "SLE PROVISION PERIOD ENDS: " <> display (run (sleSIIBuilder sii))

protocolAbortCB :: (HasLogFunc env) => env -> SleProtocolAbortHandler
protocolAbortCB state msg = runRIO state $ do
    logWarn $ "SLE PROTOCOL ABORT: " <> display msg


bindCB :: (HasLogFunc env) => env -> SleBindHandler
bindCB state sii _initiator _port _service _version = runRIO state $ do
    logDebug $ "SLE BIND: " <> display (run (sleSIIBuilder sii))
    pure Nothing


unbindCB :: (HasLogFunc env) => env -> SleUnbindHandler
unbindCB state sii _service _reason = runRIO state $ do
    logDebug $ "SLE BIND: " <> display (run (sleSIIBuilder sii))


cltuStartCB :: env -> SleCLTUStartHandler
cltuStartCB state _sii _cltuID _start _stop = runRIO state $ pure Nothing


rafStartCB :: env -> SleRAFStartHandler
rafStartCB state _sii _start _stop _reqQual = runRIO state $ pure Nothing

rcfStartCB :: env -> SleRCFStartHandler
rcfStartCB state _sii _start _stop _reqQual = runRIO state $ pure Nothing


cltuTransDataCB :: env -> SleCLTUTransferDataHandler
cltuTransDataCB state _cltuID _cltu = runRIO state $ pure (Nothing, 0)



cltuNegTransCB :: env -> SleCLTUNegTransferHandler
cltuNegTransCB state _cltuID _msg = runRIO state $ pure 0




stopCB :: (HasLogFunc env) => env -> SleStopHandler
stopCB state sii appID = runRIO state $ do
    logInfo
        $  "SLE STOP: "
        <> display (run (sleSIIBuilder sii))
        <> " AppID: "
        <> display appID


throwCB :: env -> SleThrowEventHandler
throwCB state _evID _invocID _qualifier = runRIO state $ pure Nothing
