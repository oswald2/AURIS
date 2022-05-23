{-# LANGUAGE CPP #-}
module Application.Chains
    ( runChains
    , runTMChain
    , runTMNctrsChain
    , runTMCnCChain
    , runTCCnCChain
    , runEdenChain
    , runTCChain
    , NctrsID(..)
    , CncID(..)
    , EdenID(..)
    ) where

import           Conduit
import           Conduit.SocketConnector        ( runGeneralTCPReconnectClient )
import           Data.Conduit.Network
import           Data.Conduit.TQueue            ( sinkTBQueue
                                                , sourceTBQueue
                                                )
import           RIO
import qualified RIO.HashMap                   as HM

import           Data.PUS.Config
import           Data.PUS.Events
import           Data.PUS.ExtractedPUSPacket    ( ExtractedPacket )
import           Data.PUS.GlobalState
import           Data.PUS.MissionSpecific.Definitions
                                                ( PUSMissionSpecific )
import           Data.PUS.NcduToTMFrame         ( ncduToTMFrameC )
import           Data.PUS.PUSPacketEncoder      ( tcPktToEncPUSC )
import           Data.PUS.TCPacketEncoder       ( tcPktEncoderC )
import           Data.PUS.TCTransferFrame       ( tcFrameEncodeC )
import           Data.PUS.TCTransferFrameEncoder
                                                ( tcFrameToCltuC
                                                , tcSegmentToTransferFrame
                                                )
import           Data.PUS.TMFrameExtractor
import           Data.PUS.TMFrame 
import           Data.PUS.TMPacketProcessing    ( packetProcessorC
                                                , raiseTMPacketC
                                                , raiseTMParameterC
                                                , storeTMPacketC
                                                )

import           Data.PUS.CLTU                  ( cltuEncodeC
                                                , cltuEncodeRandomizedC
                                                )
import           Data.PUS.CLTUEncoder           ( cltuToNcduC )
import           Data.PUS.Counter               ( initialSSCCounterMap )
import           Data.PUS.SegmentEncoder        ( tcSegmentEncoderC )
import           Data.PUS.Statistics

import           Protocol.CnC                   ( cncProcessAcks
                                                , cncToTMPacket
                                                , receiveCnCC
                                                , sendTCCncC
                                                )
import           Protocol.EDEN                  ( encodeEdenMessageC
                                                , receiveEdenMessageC
                                                )
import           Protocol.EDENEncoder           ( createEdenMsgC )
import           Protocol.EDENProcessor         ( edenMessageProcessorC )
import           Protocol.NCTRSProcessor        ( encodeTcNcduC
                                                , nctrsProcessorC
                                                , receiveAdminNcduC
                                                , receiveTcNcduC
                                                , receiveTmNcduC
                                                )
import           Protocol.NDIULiteProcessor                                             
import           Protocol.ProtocolInterfaces    ( ConnType
                                                    ( ConnAdmin
                                                    , ConnSingle
                                                    , ConnTC
                                                    , ConnTM
                                                    )
                                                , ConnectionState
                                                    ( Connected
                                                    , Disconnected
                                                    )
                                                , ProtocolInterface
                                                    ( IfCnc
                                                    , IfEden
                                                    , IfNctrs
                                                    , IfNdiu
                                                    )
                                                )
import           Protocol.ProtocolSwitcher      ( InterfaceSwitcherMap
                                                , ProtocolQueue
                                                , createInterfaceChannel
                                                , receiveCltuChannelC
                                                , receivePktChannelC
                                                , receiveQueueMsg
                                                , switchProtocolCltuC
                                                , switchProtocolFrameC
                                                , switchProtocolPktC
                                                )

import           Control.PUS.Classes

import           Data.Time.Clock.POSIX

#ifdef HAS_SLE 
import           Protocol.SLE
#endif

newtype NctrsID = NctrsID Word16
newtype CncID = CncID Word16
newtype EdenID = EdenID Word16


tmPacketQueueSize :: Natural
tmPacketQueueSize = 5000


runTMNctrsChain
    :: NctrsConfig -> SwitcherMap -> RIO GlobalState ()
runTMNctrsChain cfg vcMap = do
    logDebug "runTMNctrsChain entering"

    let chain =
            receiveTmNcduC
                .| ncduToTMFrameC (IfNctrs (cfgNctrsID cfg))
                .| tmFrameSwitchVC vcMap

    runGeneralTCPReconnectClient
        (clientSettings (fromIntegral (cfgNctrsPortTM cfg))
                        (encodeUtf8 (cfgNctrsHost cfg))
        )
        200000
        (tmClient chain)

    logDebug "runTMNctrsChain leaving"
  where
    tmClient chain app = do
        raiseEvent
            (EVAlarms
                (EVEConnection (IfNctrs (cfgNctrsID cfg)) ConnTM Connected)
            )
        logInfo $ "Connected TM connection on NCTRS " <> display
            (cfgNctrsID cfg)

        res <- try $ void $ runConduitRes (appSource app .| chain)

        raiseEvent
                (EVAlarms
                    (EVEConnection (IfNctrs (cfgNctrsID cfg))
                                   ConnTM
                                   Disconnected
                    )
                )
        case res of
            Left (e :: SomeException) -> do
                logError
                    $  display @Text "NCTRS Interface Exception: "
                    <> displayShow e
                throwM e
            Right _ -> do
                logWarn $ "Disconnected TM connection on NCTRS " <> display
                    (cfgNctrsID cfg)
                return ()



runTCNctrsChain :: NctrsConfig -> ProtocolQueue -> RIO GlobalState ()
runTCNctrsChain cfg cltuQueue = do
    logDebug "runTCNctrsChain entering"

    let chain = receiveCltuChannelC cltuQueue .| cltuToNcduC .| encodeTcNcduC
        recvChain = receiveTcNcduC .| nctrsProcessorC

    runGeneralTCPReconnectClient
        (clientSettings (fromIntegral (cfgNctrsPortTC cfg))
                        (encodeUtf8 (cfgNctrsHost cfg))
        )
        200000
        (\app -> race_ (tcClient chain app) (tcRecvClient recvChain app))

    logDebug "runTCNctrsChain leaving"
  where
    tcClient chain app = do
        raiseEvent (EVAlarms (EVEConnection (IfNctrs (cfgNctrsID cfg)) ConnTC Connected))
        logInfo $ "Connected TC connection on NCTRS " <> display
            (cfgNctrsID cfg)

        res <- try $ void $ runConduitRes (chain .| appSink app)

        raiseEvent (EVAlarms (EVEConnection (IfNctrs (cfgNctrsID cfg))
                                   ConnTC
                                   Disconnected))
            
        case res of
            Left (e :: SomeException) -> do
                logError
                    $  display @Text "NCTRS Interface Exception: "
                    <> displayShow e
                throwM e
            Right _ -> do
                logWarn $ "Disconnected TC connection on NCTRS " <> display
                    (cfgNctrsID cfg)
                return ()
    tcRecvClient chain app = do
        res <- try $ void $ runConduitRes (appSource app .| chain)
        case res of
            Left (e :: SomeException) -> do
                logError
                    $  display @Text "NCTRS Interface Exception: "
                    <> displayShow e
                throwM e
            Right _ -> do
                logWarn $ "Disconnected TC connection on NCTRS " <> display
                    (cfgNctrsID cfg)
                return ()

runAdminNctrsChain :: NctrsConfig -> RIO GlobalState ()
runAdminNctrsChain cfg = do
    logDebug "runAdminNctrsChain entering"

    let chain = receiveAdminNcduC .| printC

    runGeneralTCPReconnectClient
        (clientSettings (fromIntegral (cfgNctrsPortADM cfg))
                        (encodeUtf8 (cfgNctrsHost cfg))
        )
        200000
        (tmClient chain)

    logDebug "runAdminNctrsChain leaving"
  where
    tmClient chain app = do
        raiseEvent
            (EVAlarms
                (EVEConnection (IfNctrs (cfgNctrsID cfg)) ConnAdmin Connected)
            )
        logInfo $ "Connected ADMIN connection on NCTRS " <> display
            (cfgNctrsID cfg)
        res <- try $ void $ runConduitRes (appSource app .| chain)

        raiseEvent
                (EVAlarms
                    (EVEConnection (IfNctrs (cfgNctrsID cfg))
                                   ConnAdmin
                                   Disconnected
                    )
                )

        case res of
            Left (e :: SomeException) -> do
                logError
                    $  display @Text "NCTRS Interface Exception: "
                    <> displayShow e
                throwM e
            Right _ -> do
                logWarn $ "Disconnected ADMIN connection on NCTRS " <> display
                    (cfgNctrsID cfg)
                return ()


runTMCnCChain
    :: CncConfig
    -> PUSMissionSpecific
    -> TBQueue ExtractedPacket
    -> RIO GlobalState ()
runTMCnCChain cfg missionSpecific pktQueue = do
    logDebug "runTMCnCChain entering"

    let chain =
            receiveCnCC missionSpecific (IfCnc (cfgCncID cfg))
                .| cncToTMPacket (IfCnc (cfgCncID cfg))
                .| packetStatC
                .| sinkTBQueue pktQueue

    runGeneralTCPReconnectClient
        (clientSettings (fromIntegral (cfgCncPortTM cfg))
                        (encodeUtf8 (cfgCncHost cfg))
        )
        200000
        (tmClient chain)

    logDebug "runTMCnCChain leaving"
  where
    tmClient chain app = do
        raiseEvent
            (EVAlarms (EVEConnection (IfCnc (cfgCncID cfg)) ConnTM Connected))
        logInfo $ "Connected TM connection on C&C " <> display (cfgCncID cfg)

        res <- try $ void $ runConduitRes (appSource app .| chain)

        raiseEvent
                (EVAlarms
                    (EVEConnection (IfCnc (cfgCncID cfg)) ConnTM Disconnected)
                )

        case res of
            Left (e :: SomeException) -> do
                logError
                    $  display @Text "C&C TM Interface Exception: "
                    <> displayShow e
                throwM e
            Right _ -> do
                logWarn $ "Disconnected TM connection on C&C " <> display
                    (cfgCncID cfg)
                return ()


runTCCnCChain
    :: CncConfig
    -> PUSMissionSpecific
    -> ProtocolQueue
    -> TBQueue ExtractedPacket
    -> RIO GlobalState ()
runTCCnCChain cfg missionSpecific duQueue pktQueue = do
    logDebug "runTCCnCChain entering"

    let chain = receivePktChannelC duQueue .| sendTCCncC
        ackChain =
            receiveCnCC missionSpecific ifID .| cncProcessAcks ifID pktQueue
        ifID = IfCnc (cfgCncID cfg)

    logDebug
        $  "C&C TC: connecting to: "
        <> display (cfgCncHost cfg)
        <> " "
        <> display (cfgCncPortTC cfg)

    runGeneralTCPReconnectClient
        (clientSettings (fromIntegral (cfgCncPortTC cfg))
                        (encodeUtf8 (cfgCncHost cfg))
        )
        200000
        (\app -> race_ (tcClient ifID chain app) (tcAckClient ifID ackChain app)
        )

    logDebug "runTCCnCChain leaving"
  where
    tcClient ifID chain app = do
        raiseEvent (EVAlarms (EVEConnection ifID ConnTC Connected))
        logInfo $ "Connected TC connection on C&C " <> display (cfgCncID cfg)

        res <- try $ void $ runConduitRes (chain .| appSink app)

        raiseEvent
                (EVAlarms
                    (EVEConnection (IfCnc (cfgCncID cfg)) ConnTC Disconnected)
                )

        case res of
            Left (e :: SomeException) -> do
                logError
                    $  display @Text "C&C TC Interface Exception: "
                    <> displayShow e
                throwM e
            Right _ -> do
                logWarn $ "Disconnected TC connection on C&C " <> display
                    (cfgCncID cfg)
                return ()
    tcAckClient ifID chain app = do
        logDebug "C&C TC Ack reader thread started"

        res <- try $ void $ runConduitRes (appSource app .| chain)

        raiseEvent (EVAlarms (EVEConnection ifID ConnTC Disconnected))
        case res of
            Left (e :: SomeException) -> do
                logError
                    $  display @Text "C&C TC Interface Exception: "
                    <> displayShow e
                throwM e
            Right _ -> do
                logWarn $ "Disconnected TC connection on C&C " <> display
                    (cfgCncID cfg)
                return ()
        logDebug "C&C TC Ack reader thread leaves"






runEdenChain
    :: EDENConfig
    -> PUSMissionSpecific
    -> TBQueue ExtractedPacket
    -> ProtocolQueue
    -> RIO GlobalState ()
runEdenChain cfg missionSpecific pktQueue edenQueue = do
    logDebug "runEdenChain entering"


    void $ runGeneralTCPReconnectClient
        (clientSettings (fromIntegral (cfgEdenPort cfg))
                        (encodeUtf8 (cfgEdenHost cfg))
        )
        200000
        edenClient

    logDebug "runEdenChain leaving"
  where
    edenClient app = do
        raiseEvent
            (EVAlarms
                (EVEConnection (IfEden (cfgEdenID cfg)) ConnSingle Connected)
            )
        logInfo $ "Connected on EDEN " <> display (cfgEdenID cfg)

        res <- try $ race_ (tmChain app) (tcChain app)

        raiseEvent
                (EVAlarms
                    (EVEConnection (IfEden (cfgEdenID cfg))
                                   ConnSingle
                                   Disconnected
                    )
                )

        case res of
            Left (e :: SomeException) -> do
                logError
                    $  display @Text "EDEN Interface Exception: "
                    <> displayShow e
                throwM e
            Right _ -> do
                logWarn $ "Disconnected on EDEN " <> display (cfgEdenID cfg)
                return ()

    tmChain app = do
        let chain =
                appSource app
                    .| receiveEdenMessageC
                    .| edenMessageProcessorC missionSpecific
                                             (IfEden (cfgEdenID cfg))
                    .| packetStatC
                    .| sinkTBQueue pktQueue
        runConduitRes chain

    tcChain app = do
        let chain =
                receiveQueueMsg edenQueue
                    .| createEdenMsgC
                    .| encodeEdenMessageC
                    .| appSink app
        runConduitRes chain


runNdiu
    :: TMFrameConfig 
    -> NDIULiteConfig
    -> PUSMissionSpecific
    -> SwitcherMap
    -> ProtocolQueue
    -> RIO GlobalState ()
runNdiu tmFrameCfg cfg _missionSpecific vcMap ndiuQueue = do
    logDebug "runNdiuChain entering"

    queue <- newTBQueueIO 500

    race_ (runNdiuChain tmFrameCfg cfg vcMap queue) (conversionThread queue)

    logDebug "runNdiuChain leaving"
    where 
        conversionThread queue = do 
            runConduitRes $ receiveQueueMsg ndiuQueue .| createNdiuC .| sinkTBQueue queue





runTMChain
    :: PUSMissionSpecific -> SwitcherMap -> TBQueue ExtractedPacket -> RIO GlobalState ()
runTMChain missionSpecific vcMap pktQueue = do
    logDebug "runTMChain entering"

    cfg <- view getConfig
    let nctrsCfg = cfgNCTRS cfg
        cncCfg   = cfgCnC cfg

    let chain =
            sourceTBQueue pktQueue
                .| packetProcessorC
                .| storeTMPacketC
                .| raiseTMPacketC
                .| raiseTMParameterC
                .| sinkNull

    let processingThread = conc $ runConduitRes chain
        nctrsTMThread conf = conc $ runTMNctrsChain conf vcMap
        cncTMThread conf = conc $ runTMCnCChain conf missionSpecific pktQueue

        threads =
            processingThread
                :  fmap nctrsTMThread nctrsCfg
                <> fmap cncTMThread   cncCfg

    -- run all threads and wait until all are finished
    runConc $ mconcat threads

    logDebug "runTMCain leaving"




runTCChain :: PUSMissionSpecific -> InterfaceSwitcherMap -> RIO GlobalState ()
runTCChain missionSpecific switcherMap = do
    logDebug "runTCChain entering"

    rqstQueue <- view getRqstQueue
    cfg       <- view getConfig

    let cltuChain = if cfgRandomizerEnabled cfg
            then cltuEncodeRandomizedC
            else cltuEncodeC
        rqstChain =
            sourceTBQueue rqstQueue
                .| concatC
                .| tcPktEncoderC missionSpecific
                .| tcPktToEncPUSC initialSSCCounterMap
                .| switchProtocolPktC switcherMap
-- TODO: This chain is currently only BD mode! AD mode needs to be implemented
                .| tcSegmentEncoderC
                .| tcSegmentToTransferFrame
                .| tcFrameEncodeC
                .| switchProtocolFrameC switcherMap
                .| tcFrameToCltuC
                .| cltuChain
                .| switchProtocolCltuC switcherMap

    let rqstThread = conc $ runConduitRes rqstChain

    -- start all threads and wait until they are all finished
    runConc rqstThread

    logDebug "runTCChain leaving"



-- | This is the main function to start the processing chains. Runs the TM 
-- and TC interfaces and processing chains in several threads
runChains :: PUSMissionSpecific -> RIO GlobalState ()
runChains missionSpecific = do
    logDebug "runChains enters"

    cfg                         <- view getConfig

    pktQueue                    <- newTBQueueIO tmPacketQueueSize

    (_vcThread, vcMap) <- setupFrameSwitcher pktQueue


    -- create the EDEN interface threads and the switcher map 
    (switcherMap1, edenThreads) <- foldM (edenIf pktQueue)
                                         (HM.empty, mempty)
                                         (cfgEDEN cfg)

    -- create the interface threads and switcher map for the NCTRS interfaces
    (switcherMap2, nctrsThreads) <- foldM nctrsIf
                                          (switcherMap1, edenThreads)
                                          (cfgNCTRS cfg)

    -- create the interface threads and switcher map for the C & C interfaces
    (switcherMap3, cncThreads) <- foldM (cncIf pktQueue)
                                             (switcherMap2, nctrsThreads)
                                             (cfgCnC cfg)

    -- create the interface threads and switcher map for the C & C interfaces
    (switcherMap, interfaceThreads) <- foldM (ndiuIf cfg vcMap)
                                             (switcherMap3, cncThreads)
                                             (cfgNDIU cfg)

#ifdef HAS_SLE 
    env <- ask
    sleThreads <- case cfgSLE cfg of
        Just sleCfg -> do 
            logInfo "Starting SLE interface..."
            pure $ conc $ startSLE sleCfg vcMap (getSleCmdQueue env) 
        Nothing -> pure mempty 
#else 
    let sleThreads = mempty 
#endif 

    let tmThreads    = conc $ runTMChain missionSpecific vcMap pktQueue
        tcThreads    = conc $ runTCChain missionSpecific switcherMap
        adminThreads = mconcat $ map (conc . runAdminNctrsChain) (cfgNCTRS cfg)
        stats        = conc $ statThread

    runConc
        (tmThreads <> tcThreads <> interfaceThreads <> adminThreads <> sleThreads <> stats)

    logDebug "runChains leaves"
  where
    edenIf pktQueue (switcherMap, ts) x = do
        (queue, newSm) <- createInterfaceChannel switcherMap
                                                 (IfEden (cfgEdenID x))
        return
            (newSm, ts <> conc (runEdenChain x missionSpecific pktQueue queue))
    nctrsIf (sm, ts) x = do
        (queue, newSm) <- createInterfaceChannel sm (IfNctrs (cfgNctrsID x))
        return (newSm, ts <> conc (runTCNctrsChain x queue))
    cncIf pktQueue (sm, ts) x = do
        (queue, newSm) <- createInterfaceChannel sm (IfCnc (cfgCncID x))
        return
            (newSm, ts <> conc (runTCCnCChain x missionSpecific queue pktQueue))
    ndiuIf cfg vcMap (switcherMap, ts) x = do
        (queue, newSm) <- createInterfaceChannel switcherMap
                                                 (IfNdiu (cfgNdiuID x))
        return
            (newSm, ts <> conc (runNdiu (cfgTMFrame cfg) x missionSpecific vcMap queue))






statThread
    :: ( MonadIO m
       , MonadReader env m
       , HasStats env
       , HasRaiseEvent env
       , HasLogFunc env
       )
    => m ()
statThread = do
    env <- ask
    logDebug "Statistics thread starting..."
    go env Nothing Nothing
  where
    go env oldFrameStats oldPktStats = do
        threadDelay 2000000
        let frameVar  = getFrameStats env
            packetVar = getPacketStats env
        frameStats <- readTVarIO frameVar
        now        <- liftIO getPOSIXTime
        fr         <- case oldFrameStats of
            Nothing       -> pure $ TMFrameStats 0 0 0 0 0 nullTimeStamp
            Just oldStats -> do
                let (totalRate, totalDataRate) = statTotal frameStats
                    (rate     , dataRate     ) = statCalc frameStats oldStats
                pure $ TMFrameStats { tmStatFrameTotal      = totalRate
                                    , tmStatFrameTotalBytes = totalDataRate
                                    , tmStatFrameRate       = rate
                                    , tmStatFrameBytes      = dataRate
                                    , tmStatFramesN         = _statN frameStats
                                    , tmStatFrameTime       = TimeStamp now
                                    }

        pktStats <- readTVarIO packetVar
        pk       <- case oldPktStats of
            Nothing       -> pure $ TMPacketStats 0 0 0 0 0 nullTimeStamp
            Just oldStats -> do
                let (totalRate, totalDataRate) = statTotal pktStats
                    (rate     , dataRate     ) = statCalc pktStats oldStats
                pure $ TMPacketStats { tmStatPacketTotal      = totalRate
                                     , tmStatPacketTotalBytes = totalDataRate
                                     , tmStatPacketRate       = rate
                                     , tmStatPacketBytes      = dataRate
                                     , tmStatPacketsN         = _statN pktStats
                                     , tmStatPacketTime       = TimeStamp now
                                     }

        -- send statistics to GUI
        raiseEvent
            (EVTelemetry
                (EVTMStatistics TMStatistics { _statFrame   = fr
                                             , _statPackets = pk
                                             }
                )
            )

        go env (Just frameStats) (Just pktStats)

