{-# LANGUAGE OverloadedStrings
    , BangPatterns
    , NoImplicitPrelude
    , RecordWildCards
    , NamedFieldPuns

#-}

module Main where

import           RIO

--import qualified RIO.ByteString                as B

import           Graphics.UI.FLTK.LowLevel.FLTKHS
import qualified Graphics.UI.FLTK.LowLevel.FL  as FL
--import           Graphics.UI.FLTK.LowLevel.Fl_Types
--import           Graphics.UI.FLTK.LowLevel.Fl_Enumerations

--import           Data.Sequence                 as S

import           AURISi

import           GUI.MainWindow
import           GUI.MainWindowCallbacks
import           GUI.Colors

import           Data.PUS.PUSPacket
import           Data.PUS.EncTime
import           Data.PUS.APID
import           Data.PUS.ExtractedDU
import           Data.PUS.SegmentationFlags
import           Data.PUS.Types
import           Data.PUS.PUSDfh
import           Protocol.ProtocolInterfaces

import           GUI.PUSPacketTable


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


main :: IO ()
main = do 
    mainWindow <- ui 
    FL.run >> FL.flush

replMain :: IO ()
replMain = ui >> FL.replRun




-- pusPkt x = PUSPacket pusHdr' pusDfh' Nothing payload
--  where
--   pusHdr' = PUSHeader 0 0 PUSTM True (APID 256) SegmentStandalone (mkSSC x) 0 0
--   pusDfh' = PUSTMStdHeader 0 3 25 (mkSourceID 0) nullCUCTime
--   payload = B.pack [0 .. 255]

-- epu x = ModelValue $ ExtractedDU (toFlag Good True) Nothing IF_NCTRS x

-- pkts = S.fromList $ map (epu . pusPkt) [0 .. 10]
