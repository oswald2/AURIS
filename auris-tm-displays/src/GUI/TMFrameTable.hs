{-# LANGUAGE
  OverloadedLabels
#-}
module GUI.TMFrameTable
    ( TMFrameTable
    , createTMFrameTable
    , tmFrameTableAddRow
    , tmFrameTableSetRows
    , tmFrameTableClearRows
    , tmFrameTableSetValues
    , tmFrameTableSetCallback
    , tmFrameTableGetLatestERT
    ) where

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

import           General.Time


data TMFrameTable = TMFrameTable
    { _tmfrTable     :: TreeView
    , _tmfrModel     :: SeqStore (ExtractedDU TMFrame)
    , _tmfrSortModel :: TreeModelSort
    }

-- | Add a single row of a 'TMFrame' wrapped in a 'ExtractedDU'. Ensures, that 
-- only 'defMaxRowTM' rows are present at maximum, removes old values if the
-- size of the store is greater than this number (see "GUI.Definitions" for 
-- default GUI values). This function is intended for the live-view of incoming
-- telemetry.
tmFrameTableAddRow :: TMFrameTable -> ExtractedDU TMFrame -> IO ()
tmFrameTableAddRow g = addRowScrollingTable (_tmfrTable g) (_tmfrModel g)

tmFrameTableSetRows :: TMFrameTable -> [ExtractedDU TMFrame] -> IO ()
tmFrameTableSetRows g = setRowsSeqStore (_tmfrModel g)

tmFrameTableClearRows :: TMFrameTable -> IO ()
tmFrameTableClearRows g = seqStoreClear (_tmfrModel g)


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


tmFrameTableGetLatestERT :: TMFrameTable -> IO SunTime
tmFrameTableGetLatestERT g = do
    now <- getCurrentTime
    lst <- seqStoreToList (_tmfrModel g)
    let !res = foldr minERT now lst
    return res
  where
    minERT :: ExtractedDU TMFrame -> SunTime -> SunTime
    minERT du t = let t1 = (du ^. epERT) in min t1 t


-- | Create a 'TMFrameTable' from a 'Gtk.Builder'.
createTMFrameTable :: Gtk.Builder -> IO TMFrameTable
createTMFrameTable builder = do
    tv                    <- getObject builder "treeviewTMFrames" TreeView

    (_, model, sortModel) <- createSortedScrollingTable
        tv
        [ ( "ERT"
          , 190
          , Just (0, compareERT)
          , \pkt -> [#text := textDisplay (pkt ^. epERT)]
          )
        , ( "S/C ID"
          , 55
          , Nothing
          , \pkt ->
              [#text := textDisplay (pkt ^. epDU . tmFrameHdr . tmFrameScID)]
          )
        , ( "V/C ID"
          , 50
          , Nothing
          , \pkt ->
              [#text := textDisplay (pkt ^. epDU . tmFrameHdr . tmFrameVcID)]
          )
        , ( "VC FC"
          , 50
          , Just (1, compareVCFC)
          , \pkt ->
              [#text := textDisplay (pkt ^. epDU . tmFrameHdr . tmFrameVCFC)]
          )
        , ( "MC FC"
          , 55
          , Just (2, compareMCFC)
          , \pkt ->
              [#text := textDisplay (pkt ^. epDU . tmFrameHdr . tmFrameMCFC)]
          )
        , ( "DFH"
          , 44
          , Nothing
          , \pkt ->
              [ #text := if pkt ^. epDU . tmFrameHdr . tmFrameDfh
                    then "T"
                    else "F"
              ]
          )
        , ("SRC", 60, Nothing, \pkt -> [#text := textDisplay (pkt ^. epSource)])
        , ("Gap" , 60, Nothing, \pkt -> gapAttrs (pkt ^. epGap))
        , ("Qual", 40, Nothing, \pkt -> qualityAttrs (pkt ^. epQuality))
        ]
    return $ TMFrameTable tv model sortModel

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

compareERT :: ExtractedDU TMFrame -> ExtractedDU TMFrame -> Ordering
compareERT f1 f2 = compare (f1 ^. epERT) (f2 ^. epERT)

compareVCFC :: ExtractedDU TMFrame -> ExtractedDU TMFrame -> Ordering
compareVCFC f1 f2 = compare (f1 ^. epDU . tmFrameHdr . tmFrameVCFC)
                            (f2 ^. epDU . tmFrameHdr . tmFrameVCFC)

compareMCFC :: ExtractedDU TMFrame -> ExtractedDU TMFrame -> Ordering
compareMCFC f1 f2 = compare (f1 ^. epDU . tmFrameHdr . tmFrameMCFC)
                            (f2 ^. epDU . tmFrameHdr . tmFrameMCFC)


