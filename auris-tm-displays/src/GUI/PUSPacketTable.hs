{-# LANGUAGE OverloadedStrings
    , BangPatterns
    , GeneralizedNewtypeDeriving
    , DeriveGeneric
    , RecordWildCards
#-}
module GUI.PUSPacketTable
    ( PUSPacketTable
    , createPUSPacketTable
    , tmPUSPacketTableAddRow
    , tmPUSPacketTableAddRowSetValues
    , tmPUSPacketTableSetCallback
    ) where

import           RIO

import           GI.Gtk                        as Gtk
import           Data.GI.Gtk.ModelView.SeqStore

import           Data.PUS.PUSPacket
import           Data.PUS.PUSDfh
import           Data.PUS.ExtractedDU

import           General.Hexdump                ( hexdumpLineBS )
import           General.Types                  ( HexBytes(toBS) )

import           GUI.Utils
import           GUI.ScrollingTable


data PUSPacketTable = PUSPacketTable
    { _pptTable :: TreeView
    , _pptModel :: SeqStore (ExtractedDU PUSPacket)
    }

-- | Add a single row of a 'PUSPacket' wrapped in a 'ExtractedDU'. Ensures, that 
-- only 'defMaxRowTM' rows are present at maximum, removes old values if the
-- size of the store is greater than this number (see "GUI.Definitions" for 
-- default GUI values). This function is intended for the live-view of incoming
-- telemetry.
tmPUSPacketTableAddRow :: PUSPacketTable -> ExtractedDU PUSPacket -> IO ()
tmPUSPacketTableAddRow g = addRowScrollingTable (_pptTable g) (_pptModel g)


-- | Set the internal model to the list of given 'PUSPacket' values. In contrast
-- to 'tmFrameTableAddRow', this function does not limit the length as it is 
-- intended to be used in retrieval, which depends on the requested data size
tmPUSPacketTableAddRowSetValues
    :: PUSPacketTable -> [ExtractedDU PUSPacket] -> IO ()
tmPUSPacketTableAddRowSetValues g = setRowsSeqStore (_pptModel g)


-- | Set the callback function to be called, when a row in the table is activated
-- (which in GTK terms means double clicked). The callback must take the value as 
-- an 'ExtractedDU PUSPacket'.
tmPUSPacketTableSetCallback
    :: PUSPacketTable -> (ExtractedDU PUSPacket -> IO ()) -> IO ()
tmPUSPacketTableSetCallback g = setTreeViewCallback g _pptTable _pptModel


createPUSPacketTable :: Gtk.Builder -> IO PUSPacketTable
createPUSPacketTable builder = do
    tv <- getObject builder "treeviewTMPUSPackets" TreeView

    createScrollingTable
        tv
        PUSPacketTable
        [ ( "Generation Time"
          , 190
          , \pkt ->
              [ #text := maybe ""
                               textDisplay
                               (pusPktTime (pkt ^. epDU . pusDfh))
              ]
          )
        , ("ERT", 190, \pkt -> [#text := textDisplay (pkt ^. epERT)])
        , ( "APID"
          , 50
          , \pkt -> [#text := textDisplay (pkt ^. epDU . pusHdr . pusHdrAPID)]
          )
        , ( "T"
          , 30
          , \pkt -> [#text := textDisplay (pusType (pkt ^. epDU . pusDfh))]
          )
        , ( "ST"
          , 30
          , \pkt -> [#text := textDisplay (pusSubType (pkt ^. epDU . pusDfh))]
          )
        , ( "SSC"
          , 60
          , \pkt -> [#text := textDisplay (pkt ^. epDU . pusHdr . pusHdrSSC)]
          )
        , ("Source", 60, \pkt -> [#text := textDisplay (pkt ^. epSource)])
        , ( "Data"
          , 600
          , \pkt -> [#text := hexdumpLineBS (toBS (pkt ^. epDU . pusData))]
          )
        ]

