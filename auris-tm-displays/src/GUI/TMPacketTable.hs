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
tmPacketTableAddRow g pkt = do
  T.putStrLn "TMPacketTable: Adding packet..."
  addRowSeqStore (_tmptModel g) pkt


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
  T.putStrLn "createTMPacketTable called"
  
  tv <- getObject builder "treeviewTMPUSPackets" TreeView

  createScrollingTable
    tv
    TMPacketTable
    [ ("SPID"           , \pkt -> [#text := textDisplay (pkt ^. tmpSPID)])
    , ("Mnemonic"       , \pkt -> [#text := ST.toText (pkt ^. tmpMnemonic)])
    , ("Description"    , \pkt -> [#text := ST.toText (pkt ^. tmpDescr)])
    , ("Generation Time", \pkt -> [#text := textDisplay (pkt ^. tmpTimeStamp)])
    , ("ERT"            , \pkt -> [#text := textDisplay (pkt ^. tmpERT)])
    , ("APID"           , \pkt -> [#text := textDisplay (pkt ^. tmpAPID)])
    , ("T"              , \pkt -> [#text := textDisplay (pkt ^. tmpType)])
    , ("ST"             , \pkt -> [#text := textDisplay (pkt ^. tmpSubType)])
    , ("SSC"            , \pkt -> [#text := textDisplay (pkt ^. tmpSSC)])
    , ("VC"             , \pkt -> [#text := textDisplay (pkt ^. tmpVCID)])
    ]



