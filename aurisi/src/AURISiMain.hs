{-# LANGUAGE
  TemplateHaskell
  , DataKinds
  , TypeOperators
  , OverloadedLabels
#-}
module Main where

import           RIO
import qualified RIO.Text                      as T
import qualified Data.Text.IO                  as T

import           GUI.MainWindow
import           GUI.MainWindowCallbacks
--import           GUI.About
import           GUI.Theme

import           Options.Generic

import           AurisInterface
import           AurisProcessing
import           AurisConfig
import           AurisMissionSpecific

import           GHC.Conc
import           System.Directory               ( doesFileExist )

import qualified GI.GLib.Functions             as GI
import qualified GI.GLib.Constants             as GI
import qualified GI.Gtk                        as Gtk
import qualified Data.GI.Gtk.Threading         as Gtk

import           Version


ui :: AurisConfig -> IO MainWindow
ui cfg = do
  window <- createMainWindow cfg 
  void $ Gtk.onWidgetDestroy (_mwWindow window) Gtk.mainQuit
  Gtk.widgetShowAll (_mwWindow window)
  pure window






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
                    T.putStrLn $ "Error loading config: " <> err
                    exitFailure
                  Right c -> pure c
              else do
                T.putStrLn "Using default config"
                return defaultConfig
          Just path -> do
            T.putStrLn $ "Loading configuration from file " <> T.pack path
            res <- loadConfigJSON path
            case res of
              Left err -> do
                T.putStrLn $ "Error loading config: " <> err
                exitFailure
              Right c -> pure c

        -- need to call it once in main before the GUI is started
        void $ Gtk.init Nothing
        Gtk.setCurrentThreadAsGUIThread
        -- create the main window
        mainWindow <- ui cfg 
        setTheme
        mwSetMission mainWindow (aurisMission cfg)

        -- setup the interface
        (interface, _eventThread, coreQueue) <- initialiseInterface mainWindow

        -- Setup the callbacks. Since we need the interface there, we can 
        -- do this only here
        setupCallbacks mainWindow interface

        -- determine the mission-specific functionality
        missionSpecific   <- getMissionSpecific cfg
        -- start the processing chains

        _processingThread <- async $ runProcessing cfg
                                                   missionSpecific
                                                   (importmib opts)
                                                   interface
                                                   mainWindow
                                                   coreQueue

        void $ GI.timeoutAddSeconds GI.PRIORITY_DEFAULT 1 (mwTimerLabelCB mainWindow)
        Gtk.main



