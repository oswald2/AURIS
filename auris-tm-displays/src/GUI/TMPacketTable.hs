module GUI.TMPacketTable
  ( TMPacketTable
  , createTMPacketTable
  , tmPacketTableAddRow
  , tmPacketTableAddRowSetValues
  , tmPacketTableSetCallback
  )
where

import           RIO
import qualified Data.Text.Short as ST 

import           GI.Gtk                        as Gtk
import           Data.GI.Gtk.ModelView.SeqStore
import           Data.GI.Gtk.ModelView.CellLayout

import           Data.PUS.TMPacket

import           GUI.Utils
import           GUI.ScrollingTable 



data TMPacketTable = TMPacketTable {
  _tmptTable :: TreeView
  , _tmptModel :: SeqStore TMPacket
  }

-- | Add a single row of a 'TMPacket'. Ensures, that 
-- only 'defMaxRowTM' rows are present at maximum, removes old values if the
-- size of the store is greater than this number (see "GUI.Definitions" for 
-- default GUI values). This function is intended for the live-view of incoming
-- telemetry.
tmPacketTableAddRow :: TMPacketTable -> TMPacket -> IO ()
tmPacketTableAddRow g = addRowSeqStore (_tmptModel g) 


-- | Set the internal model to the list of given 'TMPacket' values. In contrast
-- to 'tmPacketTableAddRow', this function does not limit the length as it is 
-- intended to be used in retrieval, which depends on the requested data size
tmPacketTableAddRowSetValues :: TMPacketTable -> [TMPacket] -> IO () 
tmPacketTableAddRowSetValues g = setRowsSeqStore (_tmptModel g)


-- | Set the callback function to be called, when a row in the table is activated
-- (which in GTK terms means double clicked). The callback must take the value as 
-- a 'TMPacket'.
tmPacketTableSetCallback
  :: TMPacketTable -> (TMPacket -> IO ()) -> IO ()
tmPacketTableSetCallback g = setTreeViewCallback g _tmptTable _tmptModel 



createTMPacketTable :: Gtk.Builder -> IO TMPacketTable
createTMPacketTable builder = do
  tv    <- getObject builder "treeviewTMPUSPackets" TreeView
  model <- seqStoreNew []

  treeViewSetModel tv (Just model)

  treeViewSetHeadersVisible tv True

  -- add a couple columns
  col1  <- treeViewColumnNew
  col2  <- treeViewColumnNew
  col3  <- treeViewColumnNew
  col4  <- treeViewColumnNew
  col5  <- treeViewColumnNew
  col6  <- treeViewColumnNew
  col7  <- treeViewColumnNew
  col8  <- treeViewColumnNew
  col9  <- treeViewColumnNew
  col10 <- treeViewColumnNew

  treeViewColumnSetTitle col1  "SPID"
  treeViewColumnSetTitle col2  "Mnemonic"
  treeViewColumnSetTitle col3  "Description"
  treeViewColumnSetTitle col4  "Generation Time"
  treeViewColumnSetTitle col5  "ERT"
  treeViewColumnSetTitle col6  "APID"
  treeViewColumnSetTitle col7  "T"
  treeViewColumnSetTitle col8  "ST"
  treeViewColumnSetTitle col9  "SSC"
  treeViewColumnSetTitle col10 "VC"

  renderer1  <- cellRendererTextNew
  renderer2  <- cellRendererTextNew
  renderer3  <- cellRendererTextNew
  renderer4  <- cellRendererTextNew
  renderer5  <- cellRendererTextNew
  renderer6  <- cellRendererTextNew
  renderer7  <- cellRendererTextNew
  renderer8  <- cellRendererTextNew
  renderer9  <- cellRendererTextNew
  renderer10 <- cellRendererTextNew

  cellLayoutPackStart col1  renderer1  True
  cellLayoutPackStart col2  renderer2  True
  cellLayoutPackStart col3  renderer3  True
  cellLayoutPackStart col4  renderer4  True
  cellLayoutPackStart col5  renderer5  True
  cellLayoutPackStart col6  renderer6  True
  cellLayoutPackStart col7  renderer7  True
  cellLayoutPackStart col8  renderer8  True
  cellLayoutPackStart col9  renderer9  True
  cellLayoutPackStart col10 renderer10 True

  cellLayoutSetAttributes col1 renderer1 model $ \pkt ->
    [#text := textDisplay (pkt ^. tmpSPID)]
  cellLayoutSetAttributes col2 renderer2 model
    $ \pkt -> [#text := ST.toText (pkt ^. tmpMnemonic)]
  cellLayoutSetAttributes col3 renderer3 model
    $ \pkt -> [#text := ST.toText (pkt ^. tmpDescr)]
  cellLayoutSetAttributes col4 renderer4 model
    $ \pkt -> [#text := textDisplay (pkt ^. tmpTimeStamp)]
  cellLayoutSetAttributes col5 renderer5 model
    $ \pkt -> [#text := textDisplay (pkt ^. tmpERT)]
  cellLayoutSetAttributes col6 renderer6 model
    $ \pkt -> [#text := textDisplay (pkt ^. tmpAPID)]
  cellLayoutSetAttributes col7 renderer7 model
    $ \pkt -> [#text := textDisplay (pkt ^. tmpType)]
  cellLayoutSetAttributes col8 renderer8 model
    $ \pkt -> [#text := textDisplay (pkt ^. tmpSubType)]
  cellLayoutSetAttributes col9 renderer9 model
    $ \pkt -> [#text := textDisplay (pkt ^. tmpSSC)]
  cellLayoutSetAttributes col10 renderer10 model
    $ \pkt -> [#text := textDisplay (pkt ^. tmpVCID)]

  _ <- treeViewAppendColumn tv col1
  _ <- treeViewAppendColumn tv col2
  _ <- treeViewAppendColumn tv col3
  _ <- treeViewAppendColumn tv col4
  _ <- treeViewAppendColumn tv col5
  _ <- treeViewAppendColumn tv col6
  _ <- treeViewAppendColumn tv col7
  _ <- treeViewAppendColumn tv col8
  _ <- treeViewAppendColumn tv col9
  _ <- treeViewAppendColumn tv col10

  let g = TMPacketTable { _tmptTable = tv, _tmptModel = model }
  return g


