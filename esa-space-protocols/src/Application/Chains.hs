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

import           RIO
import qualified RIO.HashMap                   as HM
import           Conduit
import           Data.Conduit.Network
import           Data.Conduit.TQueue            ( sinkTBQueue
                                                , sourceTBQueue
                                                )
import           Conduit.SocketConnector        ( runGeneralTCPReconnectClient )

import           Data.PUS.Config
import           Data.PUS.GlobalState           
import           Data.PUS.MissionSpecific.Definitions
                                                ( PUSMissionSpecific )
import           Data.PUS.TMFrameExtractor      
import           Data.PUS.TMPacketProcessing    ( packetProcessorC
                                                , raiseTMPacketC
                                                , raiseTMParameterC
                                                )
import           Data.PUS.NcduToTMFrame         ( ncduToTMFrameC )
import           Data.PUS.Events                ( Event(EVAlarms)
                                                , EventAlarm(EVEConnection)
                                                )
import           Data.PUS.ExtractedPUSPacket    ( ExtractedPacket )
import           Data.PUS.TCPacketEncoder       ( tcPktEncoderC )
import           Data.PUS.PUSPacketEncoder      ( tcPktToEncPUSC )
import           Data.PUS.TCTransferFrame       ( tcFrameEncodeC )
import           Data.PUS.TCTransferFrameEncoder
                                                ( tcFrameToCltuC
                                                , tcSegmentToTransferFrame
                                                )

import           Data.PUS.SegmentEncoder        ( tcSegmentEncoderC )
import           Data.PUS.CLTU                  ( cltuEncodeRandomizedC )
import           Data.PUS.CLTUEncoder           ( cltuToNcduC )
import           Data.PUS.Counter               ( initialSSCCounterMap )

import           Protocol.NCTRSProcessor        ( receiveTcNcduC
                                                , receiveTmNcduC
                                                , receiveAdminNcduC
                                                , encodeTcNcduC
                                                , nctrsProcessorC
                                                )
import           Protocol.CnC                   ( receiveCnCC
                                                , cncProcessAcks
                                                , cncToTMPacket
                                                , sendTCCncC
                                                )
import           Protocol.EDEN                  ( encodeEdenMessageC
                                                , receiveEdenMessageC
                                                )
import           Protocol.EDENProcessor         ( edenMessageProcessorC )
import           Protocol.ProtocolInterfaces    ( ConnType
                                                    ( ConnSingle
                                                    , ConnAdmin
                                                    , ConnTM
                                                    , ConnTC
                                                    )
                                                , ConnectionState
                                                    ( Disconnected
                                                    , Connected
                                                    )
                                                , ProtocolInterface
                                                    ( IfCnc
                                                    , IfEden
                                                    , IfNctrs
                                                    )
                                                )
import           Protocol.ProtocolSwitcher      ( InterfaceSwitcherMap
                                                , ProtocolQueue
                                                , createInterfaceChannel
                                                , switchProtocolPktC
                                                , switchProtocolFrameC
                                                , switchProtocolCltuC
                                                , receivePktChannelC
                                                , receiveCltuChannelC
                                                , receiveQueueMsg
                                                )
import           Protocol.EDENEncoder           ( createEdenMsgC )

import           Control.PUS.Classes


newtype NctrsID = NctrsID Word16
newtype CncID = CncID Word16
newtype EdenID = EdenID Word16


tmPacketQueueSize :: Natural
tmPacketQueueSize = 5000


runTMNctrsChain :: NctrsConfig -> TBQueue ExtractedPacket -> RIO GlobalState ()
runTMNctrsChain cfg pktQueue = do
    logDebug "runTMNctrsChain entering"

    (_thread, vcMap) <- setupFrameSwitcher (IfNctrs (cfgNctrsID cfg)) pktQueue

    let chain =
            receiveTmNcduC
                .| ncduToTMFrameC
                .| storeTMFrameC
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
        env <- ask
        liftIO $ raiseEvent
            env
            (EVAlarms
                (EVEConnection (IfNctrs (cfgNctrsID cfg)) ConnTM Connected)
            )
        res <- try $ void $ runConduitRes (appSource app .| chain)

        liftIO
            (raiseEvent
                env
                (EVAlarms
                    (EVEConnection (IfNctrs (cfgNctrsID cfg))
                                   ConnTM
                                   Disconnected
                    )
                )
            )
        case res of
            Left (e :: SomeException) -> do
                logError
                    $  display @Text "NCTRS Interface Exception: "
                    <> displayShow e
                throwM e
            Right _ -> return ()



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
        env <- ask
        liftIO $ raiseEvent
            env
            (EVAlarms
                (EVEConnection (IfNctrs (cfgNctrsID cfg)) ConnTC Connected)
            )
        res <- try $ void $ runConduitRes (chain .| appSink app)

        liftIO
            (raiseEvent
                env
                (EVAlarms
                    (EVEConnection (IfNctrs (cfgNctrsID cfg))
                                   ConnTC
                                   Disconnected
                    )
                )
            )
        case res of
            Left (e :: SomeException) -> do
                logError
                    $  display @Text "NCTRS Interface Exception: "
                    <> displayShow e
                throwM e
            Right _ -> return ()
    tcRecvClient chain app = do
        res <- try $ void $ runConduitRes (appSource app .| chain)
        case res of
            Left (e :: SomeException) -> do
                logError
                    $  display @Text "NCTRS Interface Exception: "
                    <> displayShow e
                throwM e
            Right _ -> return ()

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
        env <- ask
        liftIO $ raiseEvent
            env
            (EVAlarms
                (EVEConnection (IfNctrs (cfgNctrsID cfg)) ConnAdmin Connected)
            )
        res <- try $ void $ runConduitRes (appSource app .| chain)

        liftIO
            (raiseEvent
                env
                (EVAlarms
                    (EVEConnection (IfNctrs (cfgNctrsID cfg))
                                   ConnAdmin
                                   Disconnected
                    )
                )
            )
        case res of
            Left (e :: SomeException) -> do
                logError
                    $  display @Text "NCTRS Interface Exception: "
                    <> displayShow e
                throwM e
            Right _ -> return ()


runTMCnCChain
    :: CncConfig
    -> PUSMissionSpecific
    -> TBQueue ExtractedPacket
    -> RIO GlobalState ()
runTMCnCChain cfg missionSpecific pktQueue = do
    logDebug "runTMCnCChain entering"

    let chain =
            receiveCnCC missionSpecific (IfCnc (cfgCncID cfg))
                .| cncToTMPacket (IfCnc (cfgCncID cfg)) Nothing
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
        env <- ask
        liftIO $ raiseEvent
            env
            (EVAlarms (EVEConnection (IfCnc (cfgCncID cfg)) ConnTM Connected))

        res <- try $ void $ runConduitRes (appSource app .| chain)

        liftIO
            (raiseEvent
                env
                (EVAlarms
                    (EVEConnection (IfCnc (cfgCncID cfg)) ConnTM Disconnected)
                )
            )
        case res of
            Left (e :: SomeException) -> do
                logError
                    $  display @Text "C&C TM Interface Exception: "
                    <> displayShow e
                throwM e
            Right _ -> return ()


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
            receiveCnCC missionSpecific ifID
                .| cncProcessAcks ifID pktQueue Nothing
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
        env <- ask
        liftIO $ raiseEvent env (EVAlarms (EVEConnection ifID ConnTC Connected))

        res <- try $ void $ runConduitRes (chain .| appSink app)

        liftIO
            (raiseEvent
                env
                (EVAlarms
                    (EVEConnection (IfCnc (cfgCncID cfg)) ConnTC Disconnected)
                )
            )
        case res of
            Left (e :: SomeException) -> do
                logError
                    $  display @Text "C&C TC Interface Exception: "
                    <> displayShow e
                throwM e
            Right _ -> return ()
    tcAckClient ifID chain app = do
        env <- ask
        logDebug "C&C TC Ack reader thread started"

        res <- try $ void $ runConduitRes (appSource app .| chain)

        liftIO
            (raiseEvent env (EVAlarms (EVEConnection ifID ConnTC Disconnected)))
        case res of
            Left (e :: SomeException) -> do
                logError
                    $  display @Text "C&C TC Interface Exception: "
                    <> displayShow e
                throwM e
            Right _ -> return ()
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
        env <- ask
        liftIO $ raiseEvent
            env
            (EVAlarms
                (EVEConnection (IfEden (cfgEdenID cfg)) ConnSingle Connected)
            )

        res <- try $ race_ (tmChain app) (tcChain app)

        liftIO
            (raiseEvent
                env
                (EVAlarms
                    (EVEConnection (IfEden (cfgEdenID cfg))
                                   ConnSingle
                                   Disconnected
                    )
                )
            )
        case res of
            Left (e :: SomeException) -> do
                logError
                    $  display @Text "EDEN Interface Exception: "
                    <> displayShow e
                throwM e
            Right _ -> return ()

    tmChain app = do
        let chain =
                appSource app
                    .| receiveEdenMessageC
                    .| edenMessageProcessorC missionSpecific
                                             (IfEden (cfgEdenID cfg))
                    .| sinkTBQueue pktQueue
        runConduitRes chain

    tcChain app = do
        let chain =
                receiveQueueMsg edenQueue
                    .| createEdenMsgC
                    .| encodeEdenMessageC
                    .| appSink app
        runConduitRes chain




runTMChain
    :: PUSMissionSpecific -> TBQueue ExtractedPacket -> RIO GlobalState ()
runTMChain missionSpecific pktQueue = do
    logDebug "runTMChain entering"

    cfg <- view getConfig
    let nctrsCfg = cfgNCTRS cfg
        cncCfg   = cfgCnC cfg

    let chain =
            sourceTBQueue pktQueue
                .| packetProcessorC
                .| raiseTMPacketC
                .| raiseTMParameterC
                .| sinkNull

    let processingThread = conc $ runConduitRes chain
        nctrsTMThread conf = conc $ runTMNctrsChain conf pktQueue
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

    let rqstChain =
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
                .| cltuEncodeRandomizedC
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

    -- create the EDEN interface threads and the switcher map 
    (switcherMap1, edenThreads) <- foldM (edenIf pktQueue)
                                         (HM.empty, mempty)
                                         (cfgEDEN cfg)

    -- create the interface threads and switcher map for the NCTRS interfaces
    (switcherMap2, nctrsThreads) <- foldM nctrsIf
                                          (switcherMap1, edenThreads)
                                          (cfgNCTRS cfg)

    -- create the interface threads and switcher map for the C & C interfaces
    (switcherMap, interfaceThreads) <- foldM (cncIf pktQueue)
                                             (switcherMap2, nctrsThreads)
                                             (cfgCnC cfg)

    let tmThreads    = conc $ runTMChain missionSpecific pktQueue
        tcThreads    = conc $ runTCChain missionSpecific switcherMap
        adminThreads = mconcat $ map (conc . runAdminNctrsChain) (cfgNCTRS cfg)

    runConc (tmThreads <> tcThreads <> interfaceThreads <> adminThreads)

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
