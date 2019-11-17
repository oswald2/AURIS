{-# LANGUAGE
    OverloadedStrings
    , BangPatterns
    , NoImplicitPrelude
#-}
module AurisProcessing
  ( runProcessing
  )
where

import           RIO
import qualified RIO.Text                      as T

import           Conduit
import           Data.Conduit.Network
import           Conduit.SocketConnector

import           Data.PUS.GlobalState
import           Data.PUS.MissionSpecific.Definitions
import           Data.PUS.TMFrameExtractor
import           Data.PUS.TMPacketProcessing
import           Data.PUS.NcduToTMFrame
import           Data.PUS.Events

import           Protocol.NCTRS
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
      model <- loadDataModel mibPath
      var   <- view getDataModel
      atomically $ writeTVar var model

      -- logInfo "An info message"
      -- logWarn "A warning message"
      -- logError "Error message. Very important"

      -- let l x = logWarn $ display ("Warning " :: Text) <> displayShow x
      -- mapM_ l [1..212]

      runTMChain cfg
    pure ()

ignoreConduit :: ConduitT i o (ResourceT (RIO GlobalState)) ()
ignoreConduit = awaitForever $ \_ -> pure ()


runTMChain :: AurisConfig -> RIO GlobalState ()
runTMChain cfg = do
  let chain =
        receiveTmNcduC
          .| ncduToTMFrameC
          .| storeFrameC
          .| tmFrameExtraction IF_NCTRS
          .| packetProcessorC
          .| raiseTMPacketC


  runGeneralTCPReconnectClient
    (clientSettings (aurisNctrsTMPort cfg) (encodeUtf8 (aurisNctrsHost cfg)))
    200000
    (tmClient chain)

 where
  tmClient chain app = do
    env <- ask
    liftIO $ raiseEvent env (EVAlarms EVNctrsTmConnected)
    void $ runConduitRes (appSource app .| chain .| ignoreConduit)
      `finally` liftIO (raiseEvent env (EVAlarms EVNctrsTmDisconnected))




loadDataModel
  :: (MonadUnliftIO m, MonadReader env m, HasLogFunc env)
  => Maybe FilePath
  -> m DataModel
loadDataModel opts = do
  home <- liftIO $ getHomeDirectory
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
          writeDataModel (home </> defaultMIBFile) model
          return model
    Nothing -> do
      let path = home </> defaultMIBFile
      ex <- liftIO $ doesFileExist path
      if ex
        then do
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
