module GUI.TMPacketTable
    ( TMPacketTable
    , createTMPacketTable
    , tmPacketTableAddRow
    , tmPacketTableSetValues
    , tmPacketTableSetCallback
    ) where

import           RIO
import qualified Data.Text.Short               as ST
--import qualified Data.Text.IO                  as T

import           GI.Gtk                        as Gtk
import           Data.GI.Gtk.ModelView.SeqStore
import           Data.GI.Base.Attributes        ( AttrOpTag(AttrSet) )

import           Data.PUS.TMPacket
import           Data.PUS.ExtractedDU

import           GUI.Utils
import           GUI.ScrollingTable
import           GUI.Colors


data TMPacketTable = TMPacketTable
    { _tmptTable       :: TreeView
    , _tmptModel       :: SeqStore (ExtractedDU TMPacket)
    , _tmptSortedModel :: TreeModelSort
    }

-- | Add a single row of a 'TMPacket'. Ensures, that 
-- only 'defMaxRowTM' rows are present at maximum, removes old values if the
-- size of the store is greater than this number (see "GUI.Definitions" for 
-- default GUI values). This function is intended for the live-view of incoming
-- telemetry.
tmPacketTableAddRow :: TMPacketTable -> ExtractedDU TMPacket -> IO ()
tmPacketTableAddRow g = addRowScrollingTable (_tmptTable g) (_tmptModel g)


-- | Set the internal model to the list of given 'TMPacket' values. In contrast
-- to 'tmPacketTableAddRow', this function does not limit the length as it is 
-- intended to be used in retrieval, which depends on the requested data size
tmPacketTableSetValues :: TMPacketTable -> [ExtractedDU TMPacket] -> IO ()
tmPacketTableSetValues g = setRowsSeqStore (_tmptModel g)


-- | Set the callback function to be called, when a row in the table is activated
-- (which in GTK terms means double clicked). The callback must take the value as 
-- a 'TMPacket'.
tmPacketTableSetCallback
    :: TMPacketTable -> (ExtractedDU TMPacket -> IO ()) -> IO ()
tmPacketTableSetCallback g = setTreeViewCallback g _tmptTable _tmptModel



createTMPacketTable :: Gtk.Builder -> IO TMPacketTable
createTMPacketTable builder = do
    tv                    <- getObject builder "treeviewTMPUSPackets" TreeView

    (_, model, sortModel) <- createSortedScrollingTable
        tv
        [ ( "SPID"
          , 70
          , Nothing
          , \pkt -> [#text := textDisplay (pkt ^. epDU . tmpSPID)]
          )
        , ( "Mnemonic"
          , 80
          , Nothing
          , \pkt -> [#text := ST.toText (pkt ^. epDU . tmpMnemonic)]
          )
        , ( "Description"
          , 250
          , Nothing
          , \pkt -> [#text := ST.toText (pkt ^. epDU . tmpDescr)]
          )
        , ( "Generation Time"
          , 190
          , Just (0, compareTimestamp)
          , \pkt -> [#text := textDisplay (pkt ^. epDU . tmpTimeStamp)]
          )
        , ( "ERT"
          , 190
          , Just (1, compareERT)
          , \pkt -> [#text := textDisplay (pkt ^. epDU . tmpERT)]
          )
        , ( "APID"
          , 50
          , Nothing
          , \pkt -> [#text := textDisplay (pkt ^. epDU . tmpAPID)]
          )
        , ( "T"
          , 30
          , Nothing
          , \pkt -> [#text := textDisplay (pkt ^. epDU . tmpType)]
          )
        , ( "ST"
          , 30
          , Nothing
          , \pkt -> [#text := textDisplay (pkt ^. epDU . tmpSubType)]
          )
        , ("SSC", 60, Nothing, displaySSC)
        , ( "VC"
          , 40
          , Nothing
          , \pkt -> [#text := textDisplay (pkt ^. epDU . tmpVCID)]
          )
        , ( "Source"
          , 60
          , Nothing
          , \pkt -> [#text := textDisplay (pkt ^. epDU . tmpSource)]
          )
        ]

    return $ TMPacketTable tv model sortModel

displaySSC :: ExtractedDU TMPacket -> [AttrOp CellRendererText 'AttrSet]
displaySSC pkt = case pkt ^. epGap of
    Nothing ->
        [ #backgroundSet := False
        , #foregroundSet := False
        , #text := textDisplay (pkt ^. epDU . tmpSSC)
        ]
    Just (_, _) ->
        [ #text := textDisplay (pkt ^. epDU . tmpSSC)
        , #backgroundSet := True
        , #backgroundRgba := paleYellow
        , #foregroundSet := True
        , #foregroundRgba := black
        ]


compareTimestamp :: ExtractedDU TMPacket -> ExtractedDU TMPacket -> Ordering
compareTimestamp pkt1 pkt2 =
    compare (pkt1 ^. epDU . tmpTimeStamp) (pkt2 ^. epDU . tmpTimeStamp)


compareERT :: ExtractedDU TMPacket -> ExtractedDU TMPacket -> Ordering
compareERT pkt1 pkt2 = compare (pkt1 ^. epERT) (pkt2 ^. epERT)
