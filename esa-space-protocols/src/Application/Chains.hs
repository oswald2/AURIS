module Application.Chains
  ( runTMChain
  , runTMNctrsChain
  , runTMCnCChain
  , runEdenChain
  , NctrsID(..)
  , CncID(..)
  , EdenID(..)
  )
where

import           RIO
import qualified RIO.HashMap                   as HM
import           Conduit
import           Data.Conduit.Network
import           Data.Conduit.TQueue
import           Conduit.SocketConnector

import           Data.PUS.Config
import           Data.PUS.GlobalState
import           Data.PUS.MissionSpecific.Definitions
import           Data.PUS.TMFrameExtractor
import           Data.PUS.TMPacketProcessing
import           Data.PUS.NcduToTMFrame
import           Data.PUS.Events
import           Data.PUS.ExtractedPUSPacket

import           Protocol.NCTRS
import           Protocol.CnC
import           Protocol.EDEN
import           Protocol.EDENProcessor
import           Protocol.ProtocolInterfaces
import           Protocol.ProtocolSwitcher
import           Protocol.EDENEncoder

import           Control.PUS.Classes


newtype NctrsID = NctrsID Word16
newtype CncID = CncID Word16
newtype EdenID = EdenID Word16


runTMNctrsChain :: NctrsConfig -> TBQueue ExtractedPacket -> RIO GlobalState ()
runTMNctrsChain cfg pktQueue = do
  logDebug "runTMNctrsChain entering"

  (_thread, vcMap) <- setupFrameSwitcher (IfNctrs (cfgNctrsID cfg)) pktQueue

  let chain =
        receiveTmNcduC .| ncduToTMFrameC .| storeFrameC .| tmFrameSwitchVC vcMap

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
      (EVAlarms (EVEConnection (IfNctrs (cfgNctrsID cfg)) ConnTM Connected))
    res <- try $ void $ runConduitRes (appSource app .| chain)

    liftIO
      (raiseEvent
        env
        (EVAlarms (EVEConnection (IfNctrs (cfgNctrsID cfg)) ConnTM Disconnected)
        )
      )
    case res of
      Left (e :: SomeException) -> do
        logError $ display @Text "NCTRS Interface Exception: " <> displayShow e
        throwM e
      Right _ -> return ()



runTMCnCChain
  :: CncConfig
  -> PUSMissionSpecific
  -> TBQueue ExtractedPacket
  -> RIO GlobalState ()
runTMCnCChain cfg missionSpecific pktQueue = do
  logDebug "runTMCnCChain entering"

  let chain = receiveCnCC missionSpecific (IfCnc (cfgCncID cfg))
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
        (EVAlarms (EVEConnection (IfCnc (cfgCncID cfg)) ConnTM Disconnected))
      )
    case res of
      Left (e :: SomeException) -> do
        logError $ display @Text "CnC Interface Exception: " <> displayShow e
        throwM e
      Right _ -> return ()


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
      (EVAlarms (EVEConnection (IfEden (cfgEdenID cfg)) ConnSingle Connected))

    res <- try $ race_ (tmChain app) (tcChain app)

    liftIO
      (raiseEvent
        env
        (EVAlarms
          (EVEConnection (IfEden (cfgEdenID cfg)) ConnSingle Disconnected)
        )
      )
    case res of
      Left (e :: SomeException) -> do
        logError $ display @Text "EDEN Interface Exception: " <> displayShow e
        throwM e
      Right _ -> return ()

  tmChain app = do
    let chain =
          appSource app
            .| receiveEdenMessageC
            .| edenMessageProcessorC missionSpecific (IfEden (cfgEdenID cfg))
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
  :: [NctrsConfig]
  -> [CncConfig]
  -> [EDENConfig]
  -> PUSMissionSpecific
  -> RIO GlobalState InterfaceSwitcherMap
runTMChain nctrsCfg cncCfg edenCfg missionSpecific = do
  logDebug "runTMCain entering"
  pktQueue <- newTBQueueIO 5000

  let chain =
        sourceTBQueue pktQueue
          .| packetProcessorC
          .| raiseTMPacketC
          .| raiseTMParameterC
          .| sinkNull

  let edenIf (switcherMap, ts) x = do
        (queue, newSm) <- createInterfaceChannel switcherMap
                                                 (IfEden (cfgEdenID x))
        return (newSm, ts <> conc (runEdenChain x missionSpecific pktQueue queue))

  (switcherMap, edenThreads) <- foldM edenIf (HM.empty, mempty) edenCfg

  let
    processingThread = conc $ runConduitRes chain
    nctrsTMThread conf = conc $ runTMNctrsChain conf pktQueue
    cncTMThread conf = conc $ runTMCnCChain conf missionSpecific pktQueue

    threads =
      processingThread
        :  fmap nctrsTMThread nctrsCfg
        <> fmap cncTMThread  cncCfg
        <> [edenThreads]

  runConc $ mconcat threads

  logDebug "runTMCain leaving"
  return switcherMap
