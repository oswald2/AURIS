{-# LANGUAGE
  OverloadedLabels
#-}
module GUI.TMFrameTable
  ( TMFrameTable
  , createTMFrameTable
  , tmFrameTableAddRow
  , tmFrameTableSetValues
  , tmFrameTableSetCallback
  )
where

import           RIO
--import qualified RIO.Text                      as T

import           GI.Gtk                        as Gtk
import           Data.GI.Gtk.ModelView.SeqStore

import           Data.PUS.TMFrame
import           Data.PUS.ExtractedDU
import           General.PUSTypes

import           GUI.Utils
import           GUI.Colors
import           GUI.ScrollingTable



data TMFrameTable = TMFrameTable {
  _tmfrTable :: TreeView
  , _tmfrModel :: SeqStore (ExtractedDU TMFrame)
  }

-- | Add a single row of a 'TMFrame' wrapped in a 'ExtractedDU'. Ensures, that 
-- only 'defMaxRowTM' rows are present at maximum, removes old values if the
-- size of the store is greater than this number (see "GUI.Definitions" for 
-- default GUI values). This function is intended for the live-view of incoming
-- telemetry.
tmFrameTableAddRow :: TMFrameTable -> ExtractedDU TMFrame -> IO ()
tmFrameTableAddRow g = addRowScrollingTable (_tmfrTable g) (_tmfrModel g)


-- | Set the internal model to the list of given 'TMFrame' values. In contrast
-- to 'tmFrameTableAddRow', this function does not limit the length as it is 
-- intended to be used in retrieval, which depends on the requested data size
tmFrameTableSetValues :: TMFrameTable -> [ExtractedDU TMFrame] -> IO ()
tmFrameTableSetValues g = setRowsSeqStore (_tmfrModel g)

-- | Set the callback function to be called, when a row in the table is activated
-- (which in GTK terms means double clicked). The callback must take the value as 
-- an 'ExtractedDU TMFrame'.
tmFrameTableSetCallback
  :: TMFrameTable -> (ExtractedDU TMFrame -> IO ()) -> IO ()
tmFrameTableSetCallback g = setTreeViewCallback g _tmfrTable _tmfrModel



-- | Create a 'TMFrameTable' from a 'Gtk.Builder'.
createTMFrameTable :: Gtk.Builder -> IO TMFrameTable
createTMFrameTable builder = do
  tv <- getObject builder "treeviewTMFrames" TreeView

  createScrollingTable
    tv
    TMFrameTable
    [ ("ERT", 190, \pkt -> [#text := textDisplay (pkt ^. epERT)])
    , ( "S/C ID", 55
      , \pkt -> [#text := textDisplay (pkt ^. epDU . tmFrameHdr . tmFrameScID)]
      )
    , ( "V/C ID", 50
      , \pkt -> [#text := textDisplay (pkt ^. epDU . tmFrameHdr . tmFrameVcID)]
      )
    , ( "VC FC", 50
      , \pkt -> [#text := textDisplay (pkt ^. epDU . tmFrameHdr . tmFrameVCFC)]
      )
    , ( "MC FC", 55
      , \pkt -> [#text := textDisplay (pkt ^. epDU . tmFrameHdr . tmFrameMCFC)]
      )
    , ( "DFH", 44
      , \pkt ->
        [#text := if pkt ^. epDU . tmFrameHdr . tmFrameDfh then "T" else "F"]
      )
    , ("SRC" , 60, \pkt -> [#text := textDisplay (pkt ^. epSource)])
    , ("Gap" , 60, \pkt -> gapAttrs (pkt ^. epGap))
    , ("Qual", 40, \pkt -> qualityAttrs (pkt ^. epQuality))
    ]

 where
  gapAttrs Nothing = [#backgroundSet := False, #foregroundSet := False]
  gapAttrs (Just (low, _)) =
    [ #text := textDisplay low
    , #backgroundSet := True
    , #backgroundRgba := paleYellow
    , #foregroundSet := True
    , #foregroundRgba := black
    ]
  qualityAttrs flag
    | toBool flag
    = [ #text := ("GOOD" :: Text)
      , #backgroundSet := False
      , #foregroundSet := False
      ]
    | otherwise
    = [ #text := ("BAD" :: Text)
      , #backgroundRgba := paleYellow
      , #backgroundSet := True
      , #foregroundSet := True
      , #foregroundRgba := black
      ]

