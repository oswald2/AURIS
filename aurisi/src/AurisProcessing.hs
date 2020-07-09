{-# LANGUAGE
    TypeApplications
#-}
module AurisProcessing
  ( runProcessing
  )
where

import           RIO
--import qualified Data.Text.IO                  as T
import           Data.PUS.Config
import           Data.PUS.GlobalState
import           Data.PUS.MissionSpecific.Definitions

import           Control.PUS.Classes

import           Interface.Interface
import           Interface.Events
import           Interface.CoreProcessor

import           AurisConfig

import           Persistence.Logging            ( withDatabaseLogger )

import           System.Directory
import           System.FilePath

import           GUI.MessageDisplay             ( messageAreaLogFunc )
import           GUI.MainWindow

import           Application.Chains
import           Application.DataModel



runProcessing
  :: AurisConfig
  -> PUSMissionSpecific
  -> Maybe FilePath
  -> Interface
  -> MainWindow
  -> TBQueue InterfaceAction
  -> IO ()
runProcessing cfg missionSpecific mibPath interface mainWindow coreQueue = do
  withLogging cfg mainWindow $ \logf -> do
    home <- getHomeDirectory
    let dbPath = ((home </> configPath) </>) <$> aurisDatabase cfg
    state <- newGlobalState (aurisPusConfig cfg)
                            dbPath
                            missionSpecific
                            logf
                            (ifRaiseEvent interface . EventPUS)

    runRIO state $ do
      -- first, try to load a data model or import a MIB
      logInfo "Loading Data Model..."

      let path = case mibPath of
            Just p  -> LoadFromMIB p serializedPath
            Nothing -> LoadFromSerialized serializedPath
          serializedPath = home </> configPath </> defaultMIBFile

      model <- loadDataModelDef path
      env   <- ask
      setDataModel env model

      logInfo "Initialising User Interface with Data Model..."
      liftIO $ mwInitialiseDataModel mainWindow model

      logInfo "Starting TM Chain..."
      let pusCfg   = aurisPusConfig cfg
          nctrsCfg = cfgNCTRS pusCfg
          cncCfg   = cfgCnC pusCfg
          edenCfg  = cfgEDEN pusCfg

      void $ async $ runCoreThread coreQueue

      runTMChain nctrsCfg cncCfg edenCfg missionSpecific


withLogging :: AurisConfig -> MainWindow -> (LogFunc -> IO ()) -> IO ()
withLogging cfg mainWindow app = do
  defLogOptions <- logOptionsHandle stdout True
  let logOptions =
        setLogMinLevel (convLogLevel (aurisLogLevel cfg)) defLogOptions

  withLogFunc logOptions $ \logFn -> do
    let logFn' = logFn <> messageAreaLogFunc mainWindow
    case (aurisDatabase cfg, aurisDbLogLevel cfg) of
      (Just dbPath, Just logLvl) -> withDatabaseLogger
        dbPath (convLogLevel logLvl) $ app . mappend logFn'
      _ -> app logFn'
