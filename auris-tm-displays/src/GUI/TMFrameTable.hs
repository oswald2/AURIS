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
import           Data.GI.Gtk.ModelView.CellLayout

import           Data.PUS.TMFrame
import           Data.PUS.ExtractedDU
import           General.PUSTypes

import           GUI.Utils
import           GUI.Colors
import           GUI.Definitions



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
tmFrameTableAddRow g val = do 
  let model = _tmfrModel g
  n <- seqStoreGetSize model 
  when (n > defMaxRowTM) $ do 
    seqStoreRemove model (n - 1)
  seqStorePrepend model val 


-- | Set the internal model to the list of given 'TMFrame' values. In contrast
-- to 'tmFrameTableAddRow', this function does not limit the length as it is 
-- intended to be used in retrieval, which depends on the requested data size
tmFrameTableSetValues :: TMFrameTable -> [ExtractedDU TMFrame] -> IO () 
tmFrameTableSetValues g values = do 
  let model = _tmfrModel g
  seqStoreClear model 
  mapM_ (seqStorePrepend model)  values 

-- | Set the callback function to be called, when a row in the table is activated
-- (which in GTK terms means double clicked). The callback must take the value as 
-- an 'ExtractedDU TMFrame'.
tmFrameTableSetCallback
  :: TMFrameTable -> (ExtractedDU TMFrame -> IO ()) -> IO ()
tmFrameTableSetCallback g action = do
  void $ Gtk.on (_tmfrTable g) #rowActivated $ \path _col -> do
    ipath <- treePathGetIndices path
    forM_ ipath $ \idxs -> do
      case idxs of
        (idx : _) -> do
          val <- seqStoreGetValue (_tmfrModel g) idx
          action val
        [] -> return ()


-- | Create a 'TMFrameTable' from a 'Gtk.Builder'.
createTMFrameTable :: Gtk.Builder -> IO TMFrameTable
createTMFrameTable builder = do
  tv    <- getObject builder "treeviewTMFrames" TreeView
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
  col8 <- treeViewColumnNew
  col9 <- treeViewColumnNew

  treeViewColumnSetTitle col1 "ERT"
  treeViewColumnSetTitle col2 "S/C ID"
  treeViewColumnSetTitle col3 "V/C ID"
  treeViewColumnSetTitle col4 "VC FC"
  treeViewColumnSetTitle col5 "MC FC"
  treeViewColumnSetTitle col6 "DFH"
  treeViewColumnSetTitle col7 "SRC"
  treeViewColumnSetTitle col8 "Gap"
  treeViewColumnSetTitle col9 "Qual"


  renderer1 <- cellRendererTextNew
  renderer2 <- cellRendererTextNew
  renderer3 <- cellRendererTextNew
  renderer4 <- cellRendererTextNew
  renderer5 <- cellRendererTextNew
  renderer6 <- cellRendererTextNew
  renderer7 <- cellRendererTextNew
  renderer8 <- cellRendererTextNew
  renderer9 <- cellRendererTextNew

  cellLayoutPackStart col1 renderer1 True
  cellLayoutPackStart col2 renderer2 True
  cellLayoutPackStart col3 renderer3 True
  cellLayoutPackStart col4 renderer4 True
  cellLayoutPackStart col5 renderer5 True
  cellLayoutPackStart col6 renderer6 True
  cellLayoutPackStart col7 renderer7 True
  cellLayoutPackStart col8 renderer8 True
  cellLayoutPackStart col9 renderer9 True

  cellLayoutSetAttributes col1 renderer1 model
    $ \pkt -> [#text := textDisplay (pkt ^. epERT)]
  cellLayoutSetAttributes col2 renderer2 model
    $ \pkt -> [#text := textDisplay (pkt ^. epDU . tmFrameHdr . tmFrameScID)]
  cellLayoutSetAttributes col3 renderer3 model
    $ \pkt -> [#text := textDisplay (pkt ^. epDU . tmFrameHdr . tmFrameVcID)]
  cellLayoutSetAttributes col4 renderer4 model
    $ \pkt -> [#text := textDisplay (pkt ^. epDU . tmFrameHdr . tmFrameVCFC)]
  cellLayoutSetAttributes col5 renderer5 model
    $ \pkt -> [#text := textDisplay (pkt ^. epDU . tmFrameHdr . tmFrameMCFC)]
  cellLayoutSetAttributes col6 renderer6 model $ \pkt ->
    [#text := if pkt ^. epDU . tmFrameHdr . tmFrameDfh then "T" else "F"]
  cellLayoutSetAttributes col7 renderer7 model
    $ \pkt -> [#text := textDisplay (pkt ^. epSource)]
  cellLayoutSetAttributes col8 renderer8 model $ \pkt -> gapAttrs (pkt ^. epGap)
  cellLayoutSetAttributes col9 renderer9 model
    $ \pkt -> qualityAttrs (pkt ^. epQuality)

  _ <- treeViewAppendColumn tv col1
  _ <- treeViewAppendColumn tv col2
  _ <- treeViewAppendColumn tv col3
  _ <- treeViewAppendColumn tv col4
  _ <- treeViewAppendColumn tv col5
  _ <- treeViewAppendColumn tv col6
  _ <- treeViewAppendColumn tv col7
  _ <- treeViewAppendColumn tv col8
  _ <- treeViewAppendColumn tv col9

  let g = TMFrameTable { _tmfrTable = tv, _tmfrModel = model }
  return g

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

