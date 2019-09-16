{-# LANGUAGE OverloadedStrings
    , BangPatterns
    , NoImplicitPrelude
    , RecordWildCards
    , NamedFieldPuns

#-}

module Main where

import           RIO

import qualified RIO.ByteString                as B

import           Graphics.UI.FLTK.LowLevel.FLTKHS
import qualified Graphics.UI.FLTK.LowLevel.FL  as FL
import           Graphics.UI.FLTK.LowLevel.Fl_Types
import           Graphics.UI.FLTK.LowLevel.Fl_Enumerations

import           Data.Sequence                 as S

import           TMPH

import           GUI.MainWindow
import           GUI.MainWindowCallbacks
import           GUI.PUSPacketTable
import           GUI.Colors

import           Data.PUS.PUSPacket
import           Data.PUS.EncTime
import           Data.PUS.APID
import           Data.PUS.ExtractedDU
import           Data.PUS.SegmentationFlags
import           Data.PUS.Types
import           Data.PUS.PUSDfh
import           Protocol.ProtocolInterfaces


ui :: IO ()
ui = do
  window     <- makeWindow
  mainWindow <- createMainWindow window
  setupCallbacks mainWindow
  showWidget (_mwWindow mainWindow)




pusPkt x = PUSPacket pusHdr' pusDfh' Nothing payload
 where
  pusHdr' = PUSHeader 0 0 PUSTM True (APID 256) SegmentStandalone (mkSSC x) 0 0
  pusDfh' = PUSTMStdHeader 0 3 25 (mkSourceID 0) nullCUCTime
  payload = B.pack [0 .. 255]

epu x = ExtractedDU (toFlag Good True) Nothing IF_NCTRS x

pkts = S.fromList $ map (epu . pusPkt) [0 .. 10]



createMainWindow :: MainWindowFluid -> IO MainWindow
createMainWindow MainWindowFluid {..} = do
  model <- newTVarIO pkts

  table <- setupTable _mfTableGroup model
  mcsWindowSetColor _mfWindow
  -- mcsWidgetSetColor _mfOpenFile
  -- mcsWidgetSetColor _mfSaveFile
  mcsButtonSetColor _mfArmButton
  mcsButtonSetColor _mfGoButton
  let mainWindow = MainWindow { _mwWindow       = _mfWindow
                              , _mwArmButton    = _mfArmButton
                              , _mwGoButton     = _mfGoButton
                              , _mwOpenFile     = _mfOpenFile
                              , _mwSaveFile     = _mfSaveFile
                              , _mwPacketTable = table
                              , _mwModel        = model
                              }
  pure mainWindow


main :: IO ()
main = ui >> FL.run >> FL.flush

replMain :: IO ()
replMain = ui >> FL.replRun




