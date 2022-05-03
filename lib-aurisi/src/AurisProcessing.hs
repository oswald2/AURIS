{-# LANGUAGE
    TypeApplications
#-}
module AurisProcessing
    ( runProcessing
    ) where

--import           Data.PUS.Config
import           Control.PUS.Classes            ( setDataModel )
import           Data.PUS.Events                ( EventFlag(..) )
import           Data.PUS.GlobalState           ( GlobalState
                                                    ( glsVerifCommandQueue
                                                    )
                                                , newGlobalState
                                                )
import           Data.PUS.MissionSpecific.Definitions
                                                ( PUSMissionSpecific )
import qualified Data.Text.IO                  as T
import           RIO

import           Interface.CoreProcessor        ( InterfaceAction
                                                , runCoreThread
                                                )
import           Interface.Events               ( IfEvent(EventPUS) )
import           Interface.Interface            ( Interface
                                                , ifRaiseEvent
                                                )

import           AurisConfig

import           System.Directory               ( getHomeDirectory )
import           System.FilePath                ( (</>) )

import           Application.Chains             ( runChains )
import           Application.DataModel          ( LoadFrom
                                                    ( LoadFromMIB
                                                    , LoadFromSerialized
                                                    )
                                                , loadDataModelDef
                                                )
import           Data.GI.Gtk.Threading          ( postGUIASync )
import           Data.Mongo.Processing          ( newDbState
                                                , startDbQueryThreads
                                                , startDbStoreThreads
                                                )
import           GUI.MainWindow                 ( MainWindow
                                                , mwInitialiseDataModel
                                                , mwMessageDisplay
                                                )
import           GUI.MessageDisplay             ( messageAreaLogFunc )
import           Persistence.DBQuery
import           Persistence.DbResultProcessor  ( dbResultFunc )
import           Persistence.Logging            ( logToDB )
import           Verification.Processor         ( processVerification )

-- import           Protocol.SLE


runProcessing
    :: AurisConfig
    -> PUSMissionSpecific
    -> Maybe FilePath
    -> Interface
    -> MainWindow
    -> TBQueue InterfaceAction
    -> Maybe (TBQueue DBQuery)
    -> IO ()
runProcessing cfg missionSpecific mibPath interface mainWindow coreQueue queryQueue
    = do
        defLogOptions <- logOptionsHandle stdout True
        let logOptions = setLogMinLevel (convLogLevel (aurisLogLevel cfg))
                                        defLogOptions

        -- start with the logging 
        withLogFunc logOptions $ \logFunc -> do

            let
                logf1 = logFunc
                    <> messageAreaLogFunc (mainWindow ^. mwMessageDisplay)

            T.putStrLn "Starting DB backend..."
            dbBackend <- case aurisDbConfig cfg of
                Just dbCfg -> do
                    dbState <- newDbState logf1
                    be      <- runRIO dbState $ startDbStoreThreads dbCfg
                    T.putStrLn "DB backend started..."
                    return (Just be)
                Nothing -> do
                    T.putStrLn "No DB backend was configured."
                    return Nothing

            -- Add the logging function to the GUI
            let logf = logf1 <> maybe mempty (mkLogFunc . logToDB) dbBackend

            -- Create a new 'GlobalState' for the processing
            state <- newGlobalState (aurisPusConfig cfg)
                                    missionSpecific
                                    logf
                                    (ifRaiseEvent interface . EventPUS)
                                    [EVFlagAll]
                                    dbBackend
                                    queryQueue

            void $ runRIO state $ do
                let startQueryThread = do
                        backend <- dbBackend
                        dbCfg   <- aurisDbConfig cfg
                        queue   <- queryQueue
                        return $ do
                            startDbQueryThreads dbCfg backend dbResultFunc queue
                case startQueryThread of
                    Nothing     -> return ()
                    Just action -> action

              -- first, try to load a data model or import a MIB
                logInfo "Loading Data Model..."
                home <- liftIO getHomeDirectory
                let path = case mibPath of
                        Just p  -> LoadFromMIB p serializedPath
                        Nothing -> LoadFromSerialized serializedPath
                    serializedPath =
                        home
                            </> configPathInstance (aurisInstance cfg)
                            </> defaultMIBFile

                model <- loadDataModelDef path
                env   <- ask
                setDataModel env model

                logInfo "Initialising User Interface with Data Model..."
                liftIO $ postGUIASync $ mwInitialiseDataModel mainWindow model

                -- Start the core processing thread (commands from GUI)
                void $ async $ runCoreThread coreQueue

                -- Start the TC verification processor 
                void $ async $ processVerification (glsVerifCommandQueue env)

                -- run all processing chains (TM and TC) as well as the 
                -- interface threads 
                logInfo "Starting TM and TC chains..."
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
