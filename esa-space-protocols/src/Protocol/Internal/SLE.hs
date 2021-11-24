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

import           Protocol.ProtocolInterfaces
import           Protocol.ProtocolSLE



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
    queues' <- traverse (createQueue . SleSII . sleInstanceCfgSII)
        $ cfgSleInstances sleCfg

    let cbs    = callbacks state vcMap queues
        queues = HM.fromList queues'

    withSLEUser
        (SeConfigFile (cfgSleSeConfig sleCfg))
        (ProxyConfigFile (cfgSleProxyConfig sleCfg))
        cbs
        (\sle -> race_ (processing sleCfg queues' sle) (commandThread queues'))
  where
    createQueue sii = do
        q <- liftIO $ newTBQueueIO 100
        pure (sii, q)

    commandThread queues = do
        cmd <- atomically $ readTBQueue cmdQueue
        case cmd of
            SLETerminate -> atomically $ do
                mapM_ (\q -> writeTBQueue (snd q) Terminate) queues
                -- we loop over, as we use race_ above and wait for the 
                -- interfaces to terminate. Our thread will be termianted
                -- automatically when all others are shutdown
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


convVersion :: SLEVersion -> SleVersion
convVersion SLEVersion1 = SleVersion1
convVersion SLEVersion2 = SleVersion2
convVersion SLEVersion3 = SleVersion3
convVersion SLEVersion4 = SleVersion4


startInstance
    :: (MonadUnliftIO m, MonadReader env m, HasLogFunc env, HasRaiseEvent env)
    => SLE
    -> Text
    -> SLEInstanceConfig
    -> (SleSII, TBQueue SleCmd)
    -> m ()
startInstance sle peerID (SLEInstRAF rafCfg) (_sii, queue) = do
    let version      = convVersion (cfgSleRafVersion rafCfg)
        deliveryMode = convDeliveryMode (cfgSleRafDeliveryMode rafCfg)
        sii          = SleSII (cfgSleRafSII rafCfg)
    env <- ask

    liftIO $ raiseEvent
        env
        (EVSLE
            (EVSLEInitRaf sii version peerID (cfgSleRafPort rafCfg) deliveryMode
            )
        )

    res <- withSleRAFUser sle
                          (cfgSleRafSII rafCfg)
                          version
                          (cfgSleRafPeerID rafCfg)
                          (cfgSleRafPort rafCfg)
                          Nothing
                          Nothing
                          deliveryMode
                          (cfgSleRafBufferSize rafCfg)
                          (cfgSleRafLatencyLimit rafCfg)
                          (runRAF peerID rafCfg sii queue)
    forM_ res $ \err ->
        logError
            $  "SLE Instance "
            <> display (cfgSleRafSII rafCfg)
            <> " returned: "
            <> display err
    pure ()
startInstance _ _ _ _ = pure ()


data SleCmd =
  RafBindSuccess SleSII
  | RafBindError SleSII Text
  | RafStartSuccess SleSII
  | RafStartError SleSII Text
  | Terminate
  deriving (Show)

data RafState =
    Terminated
    | Init
    | Bound
    | Active
    deriving (Show)

runRAF
    :: (MonadUnliftIO m, MonadReader env m, HasRaiseEvent env, HasLogFunc env)
    => Text
    -> SLERafConfig
    -> SleSII
    -> TBQueue SleCmd
    -> SLE
    -> m (Maybe Text)
runRAF peerID rafCfg sii queue sle = do
    env <- ask
    liftIO $ raiseEvent env (EVSLE (EVSLERafInitialised sii))
    -- initiate the bind 
    bindRes <- liftIO $ rafBind sle
                                (cfgSleRafPeerID rafCfg)
                                (cfgSleRafPort rafCfg)
                                peerID
                                (convVersion (cfgSleRafVersion rafCfg))
    case bindRes of
        Just err ->
            logError
                $  "Error on requesting SLE BIND for "
                <> display sii
                <> ": "
                <> display err
        Nothing -> loop Init
    pure Nothing

  where
    loop state = do
        cmd   <- atomically $ readTBQueue queue
        newSt <- processCmd state cmd
        case newSt of
            Terminated -> pure ()
            x          -> loop x

    processCmd state Terminate = do
        case state of
            Active -> do
                res <- liftIO $ rafStop sle
                forM_ res $ \err -> logError $ "SLE STOP: " <> display err
            Bound -> do
                res <- liftIO $ rafUnbind sle SleUBREnd
                forM_ res $ \err -> logError $ "SLE STOP: " <> display err
            _ -> pure ()
        pure Terminated

    processCmd Init (RafBindSuccess sii2) = do
        logInfo $ "BIND SUCCEEDED for" <> display sii2
        startRes <- liftIO $ rafStart sle Nothing Nothing SleRafAllFrames
        case startRes of
            Just err -> do
                logError $ "Error on requesting SLE START: " <> display err
                pure Bound
            Nothing -> do
                pure Bound
    processCmd Init (RafBindError sii2 diag) = do
        logError
            $  "BIND for "
            <> display sii2
            <> " returned error: "
            <> display diag
        pure Init

    processCmd Bound (RafStartSuccess sii2) = do
        logInfo $ "START SUCCEEDED for" <> display sii2
        pure Active

    processCmd Bound (RafStartError sii2 diag) = do
        logError
            $  "START for "
            <> display sii2
            <> " returned error: "
            <> display diag
        pure Bound

    processCmd state cmd = do
        logWarn
            $  "SLE: Illegal CMD "
            <> displayShow cmd
            <> " in state "
            <> displayShow state
        pure state

convDeliveryMode :: SLEDeliveryMode -> SleDeliveryMode
convDeliveryMode SLEOnlineComplete = SleCompleteOnline
convDeliveryMode SLEOnlineTimely   = SleTimelyOnline
convDeliveryMode SLEOffline        = SleOffline



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
    , cbPeerAbortHandler           = peerAbortCB state
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

peerAbortCB :: (HasLogFunc env) => env -> SlePeerAbortHandler
peerAbortCB state sii diag originator = runRIO state $ do
    logWarn
        $  "SLE PEER ABORT: "
        <> display sii
        <> " diagnostic: "
        <> display diag
        <> " originator: "
        <> display originator


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
        <> displayShow frame
    case decodeFrame (cfgTMFrame (state ^. getConfig)) frame of
        Left err -> do
            logError
                $  "Error decoding frame: "
                <> display (T.pack err)
                <> ": "
                <> display (HexBytes frame)
        Right tmFrame -> do
            let interf = case linkType of
                    SleRAF       -> IfSle SleRAFIf
                    SleRCF       -> IfSle SleRCFIf
                    SleCLTU      -> IfSle SleFCLTUIf
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
            SleOpBind -> case result of
                SleResultPositive -> sendToSii sii (RafBindSuccess sii)
                SleResultNegative -> sendToSii sii (RafBindError sii dat)
                _                 -> pure ()
            SleOpStart -> case result of
                SleResultPositive -> sendToSii sii (RafStartSuccess sii)
                SleResultNegative -> sendToSii sii (RafStartError sii dat)
                _                 -> pure ()
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
