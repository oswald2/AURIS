{-# LANGUAGE
  TemplateHaskell
  , DataKinds
  , TypeOperators
#-}
module Main where

import           RIO
import qualified RIO.Text                      as T
import qualified Data.Text.IO                  as T

import           Graphics.UI.FLTK.LowLevel.FLTKHS
import qualified Graphics.UI.FLTK.LowLevel.FL  as FL

import           AURISi

import           GUI.MainWindow
import           GUI.MainWindowCallbacks

import           Options.Generic

import           AurisInterface
import           AurisProcessing
import           AurisConfig
import           AurisMissionSpecific

import           Development.GitRev

import           GHC.Conc
import           System.Directory               ( doesFileExist )




aurisVersion :: Text
aurisVersion = T.concat
    [ "AURISi Version: 0.1.0.0 "
    , "Branch: "
    , $(gitBranch)
    , " "
    , $(gitHash)
    , "\ndirty: "
    , dirty
    , "\nCommit Date: "
    , $(gitCommitDate)
    ]

dirty :: Text
dirty | $(gitDirty) = "true"
      | otherwise  = "false"



ui :: IO MainWindow
ui = do
    window     <- makeWindow
    paramDetails <- makeParamDetailsWindow
    mainWindow <- createMainWindow window paramDetails
    setupCallbacks mainWindow
    showWidget (_mwWindow mainWindow)
    pure mainWindow






data Options w = Options {
    version :: w ::: Bool <?> "Print version information"
    , config :: w ::: Maybe String <?> "Specify a config file"
    , writeconfig :: w ::: Bool <?> "Write the default config to a file"
    , importmib :: w ::: Maybe FilePath <?> "Specifies a MIB directory. An import is done and the binary version of the MIB is stored for faster loading"
    }
    deriving (Generic)

instance ParseRecord (Options Wrapped)
deriving instance Show (Options Unwrapped)




main :: IO ()
main = do
    np <- getNumProcessors
    setNumCapabilities np

    opts <- unwrapRecord "AURISi"
    if version opts
        then T.putStrLn aurisVersion
        else if writeconfig opts
            then do
                writeConfigJSON defaultConfig "DefaultConfig.json"
                T.putStrLn "Wrote default config to file 'DefaultConfig.json'"
                exitSuccess
            else do
                cfg <- case config opts of
                    Nothing -> do
                        ex <- doesFileExist defaultConfigFileName
                        if ex
                            then do
                                T.putStrLn
                                    $  "Loading default config from "
                                    <> T.pack defaultConfigFileName
                                    <> "..."
                                res <- loadConfigJSON defaultConfigFileName
                                case res of
                                    Left err -> do
                                        T.putStrLn
                                            $  "Error loading config: "
                                            <> err
                                        exitFailure
                                    Right c -> pure c
                            else do
                                T.putStrLn "Using default config"
                                return defaultConfig
                    Just path -> do
                        T.putStrLn
                            $  "Loading configuration from file "
                            <> T.pack path
                        res <- loadConfigJSON path
                        case res of
                            Left err -> do
                                T.putStrLn $ "Error loading config: " <> err
                                exitFailure
                            Right c -> pure c

                -- need to call it once in main before the GUI is started
                void $ FL.lock

                -- create the main window
                mainWindow                <- ui
                -- setup the interface
                (interface, _eventThread) <- initialiseInterface mainWindow
                -- determine the mission-specific functionality
                missionSpecific           <- getMissionSpecific cfg
                -- start the processing chains
                _processingThread <- async $ runProcessing cfg
                                                           missionSpecific
                                                           (importmib opts)
                                                           interface
                                                           mainWindow
                -- run the FLTK GUI

                FL.run >> FL.flush



replMain :: IO ()
replMain = ui >> FL.replRun


