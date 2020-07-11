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

import           AurisConfig

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
  -> IO ()
runProcessing cfg missionSpecific mibPath interface mainWindow = do
  defLogOptions <- logOptionsHandle stdout True
  let logOptions =
        setLogMinLevel (convLogLevel (aurisLogLevel cfg)) defLogOptions
  withLogFunc logOptions $ \logFunc -> do
    let logf = logFunc <> messageAreaLogFunc (mainWindow ^. mwMessageDisplay)
    state <- newGlobalState (aurisPusConfig cfg)
                            missionSpecific
                            logf
                            (ifRaiseEvent interface . EventPUS)

    runRIO state $ do
      -- first, try to load a data model or import a MIB
      logInfo "Loading Data Model..."

      home <- liftIO getHomeDirectory
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

      runTMChain nctrsCfg cncCfg edenCfg missionSpecific
    pure ()






