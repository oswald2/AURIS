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
  )
where

import           RIO

import           GI.Gtk                        as Gtk
import           Data.GI.Gtk.ModelView.SeqStore
import           Data.GI.Gtk.ModelView.CellLayout

import           Data.PUS.PUSPacket
import           Data.PUS.PUSDfh
import           Data.PUS.ExtractedDU

import           General.Hexdump

import           GUI.Utils
import           GUI.ScrollingTable 


data PUSPacketTable = PUSPacketTable {
  _pptTable :: TreeView
  , _pptModel :: SeqStore (ExtractedDU PUSPacket)
  }

-- | Add a single row of a 'PUSPacket' wrapped in a 'ExtractedDU'. Ensures, that 
-- only 'defMaxRowTM' rows are present at maximum, removes old values if the
-- size of the store is greater than this number (see "GUI.Definitions" for 
-- default GUI values). This function is intended for the live-view of incoming
-- telemetry.
tmPUSPacketTableAddRow :: PUSPacketTable -> ExtractedDU PUSPacket -> IO ()
tmPUSPacketTableAddRow g = addRowSeqStore (_pptModel g) 


-- | Set the internal model to the list of given 'PUSPacket' values. In contrast
-- to 'tmFrameTableAddRow', this function does not limit the length as it is 
-- intended to be used in retrieval, which depends on the requested data size
tmPUSPacketTableAddRowSetValues :: PUSPacketTable -> [ExtractedDU PUSPacket] -> IO () 
tmPUSPacketTableAddRowSetValues g = setRowsSeqStore (_pptModel g)


-- | Set the callback function to be called, when a row in the table is activated
-- (which in GTK terms means double clicked). The callback must take the value as 
-- an 'ExtractedDU PUSPacket'.
tmPUSPacketTableSetCallback
  :: PUSPacketTable -> (ExtractedDU PUSPacket -> IO ()) -> IO ()
tmPUSPacketTableSetCallback g = setTreeViewCallback g _pptTable _pptModel 


createPUSPacketTable :: Gtk.Builder -> IO PUSPacketTable
createPUSPacketTable builder = do
  tv    <- getObject builder "treeviewTMPUSPackets" TreeView
  model <- seqStoreNew []

  treeViewSetModel tv (Just model)

  treeViewSetHeadersVisible tv True

  -- add a couple columns
  col1 <- treeViewColumnNew
  col2 <- treeViewColumnNew
  col3 <- treeViewColumnNew
  col4 <- treeViewColumnNew
  col5 <- treeViewColumnNew
  col6 <- treeViewColumnNew
  col7 <- treeViewColumnNew

  treeViewColumnSetTitle col1 "Generation Time"
  treeViewColumnSetTitle col2 "ERT"
  treeViewColumnSetTitle col3 "APID"
  treeViewColumnSetTitle col4 "T"
  treeViewColumnSetTitle col5 "ST"
  treeViewColumnSetTitle col6 "SSC"
  treeViewColumnSetTitle col7 "Data"

  renderer1 <- cellRendererTextNew
  renderer2 <- cellRendererTextNew
  renderer3 <- cellRendererTextNew
  renderer4 <- cellRendererTextNew
  renderer5 <- cellRendererTextNew
  renderer6 <- cellRendererTextNew
  renderer7 <- cellRendererTextNew

  cellLayoutPackStart col1 renderer1 True
  cellLayoutPackStart col2 renderer2 True
  cellLayoutPackStart col3 renderer3 True
  cellLayoutPackStart col4 renderer4 True
  cellLayoutPackStart col5 renderer5 True
  cellLayoutPackStart col6 renderer6 True
  cellLayoutPackStart col7 renderer7 True

  cellLayoutSetAttributes col1 renderer1 model $ \pkt ->
    [#text := maybe "" textDisplay (pusPktTime (pkt ^. epDU . pusDfh))]
  cellLayoutSetAttributes col2 renderer2 model
    $ \pkt -> [#text := textDisplay (pkt ^. epERT)]
  cellLayoutSetAttributes col3 renderer3 model
    $ \pkt -> [#text := textDisplay (pkt ^. epDU . pusHdr . pusHdrAPID)]
  cellLayoutSetAttributes col4 renderer4 model
    $ \pkt -> [#text := textDisplay (pusType (pkt ^. epDU . pusDfh))]
  cellLayoutSetAttributes col5 renderer5 model
    $ \pkt -> [#text := textDisplay (pusSubType (pkt ^. epDU . pusDfh))]
  cellLayoutSetAttributes col6 renderer6 model
    $ \pkt -> [#text := textDisplay (pkt ^. epDU . pusHdr . pusHdrSSC)]
  cellLayoutSetAttributes col7 renderer7 model
    $ \pkt -> [#text := hexdumpLineBS (pkt ^. epDU . pusData)]

  _ <- treeViewAppendColumn tv col1
  _ <- treeViewAppendColumn tv col2
  _ <- treeViewAppendColumn tv col3
  _ <- treeViewAppendColumn tv col4
  _ <- treeViewAppendColumn tv col5
  _ <- treeViewAppendColumn tv col6
  _ <- treeViewAppendColumn tv col7

  let g = PUSPacketTable { _pptTable = tv, _pptModel = model }
  return g
