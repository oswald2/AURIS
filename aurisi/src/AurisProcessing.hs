{-# LANGUAGE
    TypeApplications
#-}
module AurisProcessing
    ( runProcessing
    ) where

import           RIO
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
import           Data.GI.Gtk.Threading          ( postGUIASync )

import           Application.Chains
import           Application.DataModel

import           Verification.Processor


runProcessing
    :: AurisConfig
    -> PUSMissionSpecific
    -> Maybe FilePath
    -> Interface
    -> MainWindow
    -> TBQueue InterfaceAction
    -> IO ()
runProcessing cfg missionSpecific mibPath interface mainWindow coreQueue = do
    defLogOptions <- logOptionsHandle stdout True
    let logOptions =
            setLogMinLevel (convLogLevel (aurisLogLevel cfg)) defLogOptions
    withLogFunc logOptions $ \logFunc -> do
        let logf =
                logFunc <> messageAreaLogFunc (mainWindow ^. mwMessageDisplay)
        --let dbPath = ((home </> configPath) </>) <$> aurisDatabase cfg
        state <- newGlobalState (aurisPusConfig cfg)
                                missionSpecific
                                logf
                                (ifRaiseEvent interface . EventPUS)

        void $ runRIO state $ do
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
            liftIO $ postGUIASync $ mwInitialiseDataModel mainWindow model

            logInfo "Starting TM and TC chains..."

            -- Start the core processing thread (commands from GUI)
            void $ async $ runCoreThread coreQueue

            -- Start the TC verification processor 
            void $ async $ processVerification (glsVerifCommandQueue env)

            -- run all processing chains (TM and TC) as well as the 
            -- interface threads 
            runChains missionSpecific



-- withLogging :: AurisConfig -> MainWindow -> (LogFunc -> IO ()) -> IO ()
-- withLogging cfg mainWindow app = do
--   defLogOptions <- logOptionsHandle stdout True
--   let logOptions =
--         setLogMinLevel (convLogLevel (aurisLogLevel cfg)) defLogOptions

--   withLogFunc logOptions $ \logFn -> do
--     let logFn' = logFn <> messageAreaLogFunc mainWindow
--     case (aurisDatabase cfg, aurisDbLogLevel cfg) of
--       (Just dbPath, Just logLvl) -> withDatabaseLogger
--         dbPath (convLogLevel logLvl) $ app . mappend logFn'
--       _ -> app logFn'
