module GUI.TMPacketTable
  ( TMPacketTable
  , createTMPacketTable
  , tmPacketTableAddRow
  , tmPacketTableSetValues
  , tmPacketTableSetCallback
  )
where

import           RIO
import qualified Data.Text.Short               as ST
import qualified Data.Text.IO                  as T

import           GI.Gtk                        as Gtk
import           Data.GI.Gtk.ModelView.SeqStore

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
tmPacketTableSetValues :: TMPacketTable -> [TMPacket] -> IO ()
tmPacketTableSetValues g = setRowsSeqStore (_tmptModel g)


-- | Set the callback function to be called, when a row in the table is activated
-- (which in GTK terms means double clicked). The callback must take the value as 
-- a 'TMPacket'.
tmPacketTableSetCallback :: TMPacketTable -> (TMPacket -> IO ()) -> IO ()
tmPacketTableSetCallback g = setTreeViewCallback g _tmptTable _tmptModel



createTMPacketTable :: Gtk.Builder -> IO TMPacketTable
createTMPacketTable builder = do
  tv <- getObject builder "treeviewTMPUSPackets" TreeView

  createScrollingTable
    tv
    TMPacketTable
    [ ("SPID"           , 70, \pkt -> [#text := textDisplay (pkt ^. tmpSPID)])
    , ("Mnemonic"       , 80, \pkt -> [#text := ST.toText (pkt ^. tmpMnemonic)])
    , ("Description"    , 250, \pkt -> [#text := ST.toText (pkt ^. tmpDescr)])
    , ("Generation Time", 190, \pkt -> [#text := textDisplay (pkt ^. tmpTimeStamp)])
    , ("ERT"            , 190, \pkt -> [#text := textDisplay (pkt ^. tmpERT)])
    , ("APID"           , 50, \pkt -> [#text := textDisplay (pkt ^. tmpAPID)])
    , ("T"              , 30, \pkt -> [#text := textDisplay (pkt ^. tmpType)])
    , ("ST"             , 30, \pkt -> [#text := textDisplay (pkt ^. tmpSubType)])
    , ("SSC"            , 60, \pkt -> [#text := textDisplay (pkt ^. tmpSSC)])
    , ("VC"             , 40, \pkt -> [#text := textDisplay (pkt ^. tmpVCID)])
    , ("Source"         , 60, \pkt -> [#text := textDisplay (pkt ^. tmpSource)])
    ]



