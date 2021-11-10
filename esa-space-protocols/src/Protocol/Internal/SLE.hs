module Protocol.Internal.SLE
    ( startSLE
    ) where

import           RIO

import           SLE.Interface
import           SLE.Log
import           SLE.Types

import           Text.Builder                   ( run )

import           Data.PUS.Config



startSLE
    :: (MonadUnliftIO m, MonadReader env m, HasLogFunc env) => SLEConfig -> m ()
startSLE sleCfg = do
    state <- ask
    let cbs = callbacks state

    withSLEUser (SeConfigFile (cfgSleSeConfig sleCfg))
                (ProxyConfigFile (cfgSleProxyConfig sleCfg))
                cbs
                (processing sleCfg)

processing
    :: (MonadUnliftIO m, MonadReader env m, HasLogFunc env)
    => SLEConfig
    -> SLE
    -> m ()
processing sleCfg sle = do
    let instances = map (conc . startInstance sle) $ cfgSleInstances sleCfg
        threads   = foldr (<>) mempty instances
    runConc threads

startInstance
    :: (MonadUnliftIO m, MonadReader env m, HasLogFunc env)
    => SLE
    -> SLEInstanceConfig
    -> m ()
startInstance sle (SLEInstRAF rafCfg) = do
    res <- withSleRAFUser sle
                          (cfgSleRafSII rafCfg)
                          (cfgSleRafPeerID rafCfg)
                          (cfgSleRafPort rafCfg)
                          Nothing
                          Nothing
                          (convDeliveryMode (cfgSleRafDeliveryMode rafCfg))
                          (cfgSleRafBufferSize rafCfg)
                          (cfgSleRafLatencyLimit rafCfg)
                          (\_ -> pure Nothing)
    forM_ res $ \err -> 
            logError
                $  "SLE Instance "
                <> display (cfgSleRafSII rafCfg)
                <> " returned: "
                <> display err
    pure ()
startInstance _ _ = pure ()



convDeliveryMode :: SLEDeliveryMode -> SleDeliveryMode
convDeliveryMode SLEOnlineComplete = SleCompleteOnline
convDeliveryMode SLEOnlineTimely   = SleTimelyOnline
convDeliveryMode SLEOffline        = SleOffline



callbacks :: HasLogFunc env => env -> Callbacks
callbacks state = Callbacks
    { cbLogHandler                 = sleLog state
    , cbNotifyHandler              = sleNotify state
    , cbTraceHandler               = tracer state
    , cbUnexpectedHandler          = unexpectedCB state
    , cbAsyncNotifyHandler         = asyncCB state
    , cbPeerAbortHandler           = peerAbortCB state
    , cbTransferBufferHandler      = transferBufCB state
    , cbStatusReportHandler        = statusReportCB state
    , cbSyncNotifyHandler          = syncCB state
    , cbTransferDataHandler        = transferDataCB state
    , cbOpReturnHandler            = opReturnCB state
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
peerAbortCB state msg = runRIO state $ do
    logWarn $ "SLE PEER ABORT: " <> display (run msg)


transferBufCB :: env -> SleTransferBufferHandler
transferBufCB state _count = runRIO state $ pure ()

statusReportCB :: (HasLogFunc env) => env -> SleStatusReportHandler
statusReportCB state linkType msg = runRIO state $ do
    logInfo
        $  "SLE STATUS REPORT: Link Type: "
        <> displayShow linkType
        <> ": "
        <> display (run msg)

syncCB :: (HasLogFunc env) => env -> SleSyncNotifyHandler
syncCB state _linkType msg = runRIO state $ do
    logDebug $ "SLE ASYNC: " <> display (run msg)

transferDataCB :: (HasLogFunc env) => env -> SleTransferDataHandler
transferDataCB state linkType seqCnt ert cont frame = runRIO state $ do
    logDebug
        $  "SLE TRANSFER DATA: "
        <> displayShow linkType
        <> " SeqCount: "
        <> display seqCnt
        <> " ERT: "
        <> displayShow ert
        <> " Cont: "
        <> displayShow cont
        <> " Frame: "
        <> displayShow frame

opReturnCB :: (HasLogFunc env) => env -> SleOpReturnHandler
opReturnCB state sii seqCnt opType appID result invokeID dat =
    runRIO state $ do
        logDebug
            $  "SLE OP RETURN: "
            <> display (run (sleSIIBuilder sii))
            <> " SeqCount: "
            <> display seqCnt
            <> " OP: "
            <> displayShow opType
            <> " AppID: "
            <> displayShow appID
            <> " Result: "
            <> displayShow result
            <> " InvokeID: "
            <> displayShow invokeID
            <> " Data: "
            <> displayShow dat

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
        <> displayShow appID


throwCB :: env -> SleThrowEventHandler
throwCB state _evID _invocID _qualifier = runRIO state $ pure Nothing
