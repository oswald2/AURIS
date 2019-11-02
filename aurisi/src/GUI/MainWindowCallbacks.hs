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
import           General.APID
import           Data.PUS.ExtractedDU
import           Data.PUS.SegmentationFlags
import           General.PUSTypes
import           Data.PUS.PUSDfh
import           Protocol.ProtocolInterfaces

import           General.Time



setupCallbacks :: MainWindow -> IO ()
setupCallbacks window = do
    -- buff <- textBufferNew Nothing Nothing
    -- setBuffer (window ^. mwTextEditor) (Just buff)
  setCallback (window ^. mwTMPTab . tmpTabButtonAdd) (addCB window)
  pure ()


addCB :: MainWindow -> Ref Button -> IO ()
addCB window _btn = do
  -- now <- getCurrentTime
  -- let table = window ^. mwTMPTab . tmpTable
  --     model = window ^. mwTMPTab . tmpModel
  --     pusPkt x = PUSPacket pusHdr' pusDfh' Nothing payload
  --      where
  --       pusHdr' =
  --         PUSHeader 0 0 PUSTM True (APID 256) SegmentStandalone (mkSSC x) 0 0
  --       pusDfh' = PUSTMStdHeader 0 3 25 (mkSourceID 0) nullCUCTime
  --       payload = B.pack [0 .. 255]

  --     epu = ExtractedDU (toFlag Good True) now Nothing IF_NCTRS (VCID 0)

  -- addRow table model (epu (pusPkt 100))
  return ()
