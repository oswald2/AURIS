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
--import qualified RIO.ByteString                as B
import qualified RIO.Vector                    as V
--import           Data.Text.IO

import           Graphics.UI.FLTK.LowLevel.FLTKHS
--import qualified Graphics.UI.FLTK.LowLevel.FL as FL
--import Graphics.UI.FLTK.LowLevel.Fl_Types
--import Graphics.UI.FLTK.LowLevel.Fl_Enumerations

import           GUI.MainWindow
import           GUI.TMPacketTable

--import           Data.PUS.PUSPacket
--import           Data.PUS.EncTime
import           General.APID
--import           Data.PUS.ExtractedDU
--import           Data.PUS.SegmentationFlags
import           General.PUSTypes
--import           Data.PUS.PUSDfh
import           Data.PUS.TMPacket
--import           Protocol.ProtocolInterfaces

import           General.Time



setupCallbacks :: MainWindow -> IO ()
setupCallbacks window = do
    -- buff <- textBufferNew Nothing Nothing
    -- setBuffer (window ^. mwTextEditor) (Just buff)
  setCallback (window ^. mwTMPTab . tmpTabButtonAdd) (addCB window)
  pure ()


addCB :: MainWindow -> Ref Button -> IO ()
addCB window _btn = do
  now <- getCurrentTime
  let table  = window ^. mwTMPTab . tmpTable
      model  = window ^. mwTMPTab . tmpModel
      pusPkt = TMPacket { _tmpSPID      = SPID 1
                        , _tmpMnemonic  = "Mnemo"
                        , _tmpDescr     = "Test Packet to be added"
                        , _tmpAPID      = APID 256
                        , _tmpType      = mkPUSType 3
                        , _tmpSubType   = mkPUSSubType 25
                        , _tmpERT       = now
                        , _tmpTimeStamp = now
                        , _tmpVCID      = VCID 1
                        , _tmpSSC       = mkSSC 12
                        , _tmpParams    = V.empty
                        }

  addRow table model pusPkt
  return ()
