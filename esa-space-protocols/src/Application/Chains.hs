module Application.Chains
  ( runTMChain
  , runTMNctrsChain
  , runTMCnCChain
  , runTMEdenChain
  , NctrsID(..)
  , CncID(..)
  , EdenID(..)
  )
where

import           RIO

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

import           Control.PUS.Classes


newtype NctrsID = NctrsID Word16
newtype CncID = CncID Word16
newtype EdenID = EdenID Word16


runTMNctrsChain :: NctrsConfig -> TBQueue ExtractedPacket -> RIO GlobalState ()
runTMNctrsChain cfg pktQueue = do
  logDebug "runTMNctrsChain entering"

  (_thread, vcMap) <- setupFrameSwitcher (IfNctrs (cfgNctrsID cfg)) pktQueue

  dbConfig <- view getDatabasePath
  storeTMFrames <- cfgStoreTMFrames . glsConfig <$> ask

  let dbSink = case (dbConfig, storeTMFrames) of
        (Just dbPath, True) -> frameDbSink dbPath
        _                   -> sinkNull

  let chain = receiveTmNcduC
        .| ncduToTMFrameC
        .| getZipSink
            (  ZipSink dbSink
            *> ZipSink (tmFrameSwitchVC vcMap)
            )

  runGeneralTCPReconnectClient
    (clientSettings (fromIntegral (cfgNctrsPortTM cfg))
      (encodeUtf8 (cfgNctrsHost cfg)))
    200000
    (tmClient chain)

  logDebug "runTMNctrsChain leaving"
 where
  tmClient chain app = do
    env <- ask
    liftIO $ raiseEvent env (EVAlarms EVNctrsTmConnected)
    res <- try $ void $ runConduitRes (appSource app .| chain)

    liftIO (raiseEvent env (EVAlarms EVNctrsTmDisconnected))
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
      (encodeUtf8 (cfgCncHost cfg)))
    200000
    (tmClient chain)

  logDebug "runTMCnCChain leaving"
 where
  tmClient chain app = do
    env <- ask
    liftIO $ raiseEvent env (EVAlarms EVCncTmConnected)
    res <- try $ void $ runConduitRes (appSource app .| chain)

    liftIO (raiseEvent env (EVAlarms EVCncTmDisconnected))
    case res of
      Left (e :: SomeException) -> do
        logError $ display @Text "CnC Interface Exception: " <> displayShow e
        throwM e
      Right _ -> return ()


runTMEdenChain
  :: EDENConfig
  -> PUSMissionSpecific
  -> TBQueue ExtractedPacket
  -> RIO GlobalState ()
runTMEdenChain cfg missionSpecific pktQueue = do
  logDebug "runTMEdenChain entering"

  let chain =
        receiveEdenMessageC
          .| edenMessageProcessorC missionSpecific (IfEden (cfgEdenID cfg))
          .| sinkTBQueue pktQueue

  runGeneralTCPReconnectClient
    (clientSettings (fromIntegral (cfgEdenPort cfg))
                    (encodeUtf8 (cfgEdenHost cfg))
    )
    200000
    (tmClient chain)

  logDebug "runTMEdenChain leaving"
 where
  tmClient chain app = do
    env <- ask
    liftIO $ raiseEvent env (EVAlarms EVEdenConnected)
    res <- try $ void $ runConduitRes (appSource app .| chain)

    liftIO (raiseEvent env (EVAlarms EVEdenDisconnected))
    case res of
      Left (e :: SomeException) -> do
        logError $ display @Text "EDEN Interface Exception: " <> displayShow e
        throwM e
      Right _ -> return ()



runTMChain
  :: [NctrsConfig]
  -> [CncConfig]
  -> [EDENConfig]
  -> PUSMissionSpecific
  -> RIO GlobalState ()
runTMChain nctrsCfg cncCfg edenCfg missionSpecific = do
  logDebug "runTMCain entering"
  pktQueue <- newTBQueueIO 5000

  let chain =
        sourceTBQueue pktQueue
          .| packetProcessorC
          .| raiseTMPacketC
          .| raiseTMParameterC
          .| sinkNull


  let processingThread = conc $ runConduitRes chain
      nctrsTMThread cfg = conc $ runTMNctrsChain cfg pktQueue
      cncTMThread cfg = conc $ runTMCnCChain cfg missionSpecific pktQueue
      edenThread cfg = conc $ runTMEdenChain cfg missionSpecific pktQueue

      threads =
        processingThread
          :  fmap nctrsTMThread nctrsCfg
          <> fmap cncTMThread   cncCfg
          <> fmap edenThread    edenCfg

  runConc $ mconcat threads

  logDebug "runTMCain leaving"

