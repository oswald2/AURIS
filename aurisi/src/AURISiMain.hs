{-# LANGUAGE OverloadedStrings
    , BangPatterns
    , NoImplicitPrelude
    , RecordWildCards
    , NamedFieldPuns
    , TemplateHaskell
    , DataKinds
    , DeriveGeneric     
    , FlexibleInstances 
    , OverloadedStrings 
    , StandaloneDeriving
    , TypeOperators    
#-}
module Main where

import           RIO
--import qualified RIO.ByteString                as B
import qualified RIO.Text                      as T
import qualified Data.Text.IO                  as T

import           Graphics.UI.FLTK.LowLevel.FLTKHS
import qualified Graphics.UI.FLTK.LowLevel.FL  as FL
--import           Graphics.UI.FLTK.LowLevel.Fl_Types
--import           Graphics.UI.FLTK.LowLevel.Fl_Enumerations

--import           Data.Sequence                 as S

import           AURISi

import           GUI.MainWindow
import           GUI.MainWindowCallbacks
import           GUI.Colors

-- import           Data.PUS.PUSPacket
-- import           Data.PUS.EncTime
-- import           Data.PUS.APID
-- import           Data.PUS.ExtractedDU
-- import           Data.PUS.SegmentationFlags
-- import           Data.PUS.Types
--import           Data.PUS.PUSDfh
--import           Protocol.ProtocolInterfaces

--import           GUI.PUSPacketTable

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
  mainWindow <- createMainWindow window
  setupCallbacks mainWindow
  showWidget (_mwWindow mainWindow)
  pure mainWindow



createMainWindow :: MainWindowFluid -> IO MainWindow
createMainWindow MainWindowFluid {..} = do
  tmpTab <- createTMPTab _mfTMPTab
  mcsWindowSetColor _mfWindow
  mcsTabsSetColor _mfTabs
  -- mcsWidgetSetColor _mfOpenFile
  -- mcsWidgetSetColor _mfSaveFile
--   mcsButtonSetColor _mfArmButton
--   mcsButtonSetColor _mfGoButton
  let mainWindow = MainWindow { _mwWindow   = _mfWindow
                              , _mwOpenFile = _mfOpenFile
                              , _mwSaveFile = _mfSaveFile
                              , _mwProgress = _mfProgress
                              , _mwTabs     = _mfTabs
                              , _mwTMPTab   = tmpTab
                              }
  pure mainWindow



data Options w = Options {
    version :: w ::: Bool <?> "Documentation for the foo flag"
    , config :: w ::: Maybe String <?> "Documentation for the foo flag"
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
      void $ FL.lock

      -- create the main window
      mainWindow               <- ui
      -- setup the interface 
      (interface, _eventThread) <- initialiseInterface mainWindow
      -- determine the mission-specific functionality
      missionSpecific          <- getMissionSpecific cfg
      -- start the processing chains
      _processingThread <- async $ runProcessing cfg missionSpecific interface
      -- run the FLTK GUI
      FL.run >> FL.flush



replMain :: IO ()
replMain = ui >> FL.replRun


