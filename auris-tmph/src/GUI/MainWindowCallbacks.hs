{-# LANGUAGE OverloadedStrings
    , BangPatterns
    , GeneralizedNewtypeDeriving
    , DeriveGeneric
    , RecordWildCards
    , NoImplicitPrelude
#-}
module GUI.MainWindowCallbacks
  ( setupCallbacks
  )
where


import           RIO
import qualified RIO.ByteString                as B
--import           Data.Text.IO

import           Graphics.UI.FLTK.LowLevel.FLTKHS
--import qualified Graphics.UI.FLTK.LowLevel.FL as FL
--import Graphics.UI.FLTK.LowLevel.Fl_Types
--import Graphics.UI.FLTK.LowLevel.Fl_Enumerations

import           GUI.MainWindow
import           GUI.PUSPacketTable

import           Data.PUS.PUSPacket
import           Data.PUS.EncTime
import           Data.PUS.APID
import           Data.PUS.ExtractedDU
import           Data.PUS.SegmentationFlags
import           Data.PUS.Types
import           Data.PUS.PUSDfh
import           Protocol.ProtocolInterfaces



setupCallbacks :: MainWindow -> IO ()
setupCallbacks window = do
    -- buff <- textBufferNew Nothing Nothing
    -- setBuffer (window ^. mwTextEditor) (Just buff)
  setCallback (window ^. mwArmButton) (armCB window)
  pure ()


armCB :: MainWindow -> Ref Button -> IO ()
armCB window _btn = do
  let table = window ^. mwPacketTable
      model = window ^. mwModel
      pusPkt x = PUSPacket pusHdr' pusDfh' Nothing payload
       where
        pusHdr' =
          PUSHeader 0 0 PUSTM True (APID 256) SegmentStandalone (mkSSC x) 0 0
        pusDfh' = PUSTMStdHeader 0 3 25 (mkSourceID 0) nullCUCTime
        payload = B.pack [0 .. 255]

      epu = ExtractedDU (toFlag Good True) Nothing IF_NCTRS 

  addRow table model (epu (pusPkt 100))

