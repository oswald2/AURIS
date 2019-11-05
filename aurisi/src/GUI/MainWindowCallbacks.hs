{-# LANGUAGE OverloadedStrings
    , BangPatterns
    , GeneralizedNewtypeDeriving
    , DeriveGeneric
    , RecordWildCards
    , NoImplicitPrelude
#-}
module GUI.MainWindowCallbacks
    ( GUI.MainWindowCallbacks.setupCallbacks
    )
where


import           RIO
import qualified RIO.Text                      as T
import qualified Data.Text.IO                  as T
import qualified RIO.Vector                    as V

import           Graphics.UI.FLTK.LowLevel.FLTKHS

import           GUI.MainWindow
import           GUI.ScrollingTable

import           General.APID
import           General.PUSTypes
import           Data.PUS.TMPacket

import           General.Time



setupCallbacks :: MainWindow -> IO ()
setupCallbacks window = do
    -- buff <- textBufferNew Nothing Nothing
    -- setBuffer (window ^. mwTextEditor) (Just buff)
    setCallback (window ^. mwTMPTab . tmpTabButtonAdd) (addCB window)

    GUI.ScrollingTable.setupCallback (window ^. mwTMPTab . tmpTable)
                                     (doubleClickTMP (window ^. mwTMPTab))

    pure ()


doubleClickTMP :: TMPacketTab -> Row -> IO ()
doubleClickTMP tmp (Row row') = do
    T.putStrLn $ "Row: " <> T.pack (show row') <> " has been double-clicked."



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
