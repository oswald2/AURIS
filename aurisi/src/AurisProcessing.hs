{-# LANGUAGE
    TypeApplications
#-}
module AurisProcessing
  ( runProcessing
  )
where

import           RIO
import qualified RIO.Text                      as T

import           Conduit
import           Data.Conduit.Network
import           Data.Conduit.TQueue
import           Conduit.SocketConnector

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

import           Interface.Interface
import           Interface.Events

import           AurisConfig

import           Data.DataModel
import           Data.MIB.LoadMIB

import           System.Directory
import           System.FilePath

import           GUI.MessageDisplay             ( messageAreaLogFunc )
import           GUI.MainWindow




configPath :: FilePath
configPath = ".config/AURISi"


defaultMIBFile :: FilePath
defaultMIBFile = configPath </> "data_model.raw"




runProcessing
  :: AurisConfig
  -> PUSMissionSpecific
  -> Maybe FilePath
  -> Interface
  -> MainWindow
  -> IO ()
runProcessing cfg missionSpecific mibPath interface mainWindow = do
  defLogOptions <- logOptionsHandle stdout True
  let logOptions =
        setLogMinLevel (convLogLevel (aurisLogLevel cfg)) defLogOptions
  withLogFunc logOptions $ \logFunc -> do
    let logf = logFunc <> messageAreaLogFunc mainWindow
    state <- newGlobalState (aurisPusConfig cfg)
                            missionSpecific
                            logf
                            (ifRaiseEvent interface . EventPUS)

    runRIO state $ do
      -- first, try to load a data model or import a MIB
      logInfo "Loading Data Model..."
      model <- loadDataModel mibPath
      var   <- view getDataModel
      atomically $ writeTVar var model

      logInfo "Initialising Data Model..."
      liftIO $ mwInitialiseDataModel mainWindow model

      logInfo "Starting TM Chain..."
      runTMChain cfg missionSpecific
    pure ()

ignoreConduit :: ConduitT i o (ResourceT (RIO GlobalState)) ()
ignoreConduit = awaitForever $ \_ -> pure ()


runTMNctrsChain :: AurisConfig -> TBQueue ExtractedPacket -> RIO GlobalState ()
runTMNctrsChain cfg pktQueue = do
  logDebug "runTMNctrsChain entering"

  (_thread, vcMap) <- setupFrameSwitcher IF_NCTRS pktQueue

  let chain =
        receiveTmNcduC .| ncduToTMFrameC .| storeFrameC .| tmFrameSwitchVC vcMap

  runGeneralTCPReconnectClient
    (clientSettings (aurisNctrsTMPort cfg) (encodeUtf8 (aurisNctrsHost cfg)))
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
  :: AurisConfig
  -> PUSMissionSpecific
  -> TBQueue ExtractedPacket
  -> RIO GlobalState ()
runTMCnCChain cfg missionSpecific pktQueue = do
  logDebug "runTMCnCChain entering"

  let chain = receiveCnCC missionSpecific .| sinkTBQueue pktQueue

  runGeneralTCPReconnectClient
    (clientSettings (aurisCnCTMPort cfg) (encodeUtf8 (aurisCnCHost cfg)))
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
  :: AurisConfig
  -> PUSMissionSpecific
  -> TBQueue ExtractedPacket
  -> RIO GlobalState ()
runTMEdenChain cfg missionSpecific pktQueue = do
  logDebug "runTMEdenChain entering"

  let chain =
        receiveEdenMessageC .| edenMessageProcessorC missionSpecific .| sinkTBQueue pktQueue

  runGeneralTCPReconnectClient
    (clientSettings (aurisEdenPort cfg) (encodeUtf8 (aurisEdenHost cfg)))
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



runTMChain :: AurisConfig -> PUSMissionSpecific -> RIO GlobalState ()
runTMChain cfg missionSpecific = do
  logDebug "runTMCain entering"
  pktQueue <- newTBQueueIO 5000

  let chain =
        sourceTBQueue pktQueue
          .| packetProcessorC
          .| raiseTMPacketC
          .| raiseTMParameterC
          .| ignoreConduit


  let processingThread = conc $ runConduitRes chain
      nctrsTMThread = conc $ runTMNctrsChain cfg pktQueue
      cncTMThread = conc $ runTMCnCChain cfg missionSpecific pktQueue
      edenThread = conc $ runTMEdenChain cfg missionSpecific pktQueue


  runConc $ 
    processingThread <> nctrsTMThread <> cncTMThread <> edenThread

  logDebug "runTMCain leaving"




loadDataModel
  :: (MonadUnliftIO m, MonadReader env m, HasLogFunc env)
  => Maybe FilePath
  -> m DataModel
loadDataModel opts = do
  home <- liftIO getHomeDirectory
  case opts of
    Just str -> do
      res <- loadMIB str
      case res of
        Left err -> do
          logError $ display ("Error on importing MIB: " :: Text) <> display err
          return Data.DataModel.empty
        Right model -> do
          logInfo $ display ("Successfully imported MIB." :: Text)
          liftIO $ createDirectoryIfMissing True (home </> configPath)
          logInfo "Writing data model to disk..."
          writeDataModel (home </> defaultMIBFile) model
          logInfo "Data Model written."
          return model
    Nothing -> do
      let path = home </> defaultMIBFile
      ex <- liftIO $ doesFileExist path
      if ex
        then do
          logDebug "calling readDataModel..."
          res <- readDataModel path
          case res of
            Left err -> do
              logError
                $  display ("Error loading data model from " :: Text)
                <> display (T.pack path)
                <> display (": " :: Text)
                <> display err
              return Data.DataModel.empty
            Right model -> do
              logInfo $ display ("Successfully loaded data model" :: Text)
              return model
        else do
          logInfo
            $  display ("Data model file '" :: Text)
            <> display (T.pack path)
            <> display ("' does not exist." :: Text)
          return Data.DataModel.empty
