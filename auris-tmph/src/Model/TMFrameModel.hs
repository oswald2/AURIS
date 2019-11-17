{-# OPTIONS_GHC -fno-warn-orphans #-}
module Model.TMFrameModel
  ( TMFrameModel
  , ToCellText(..)
  )
where

import           RIO

import           Data.PUS.ExtractedDU
import           Data.PUS.TMFrame

import           Model.ScrollingTableModel

import           Graphics.UI.FLTK.LowLevel.Fl_Enumerations

import           GUI.Colors



type TMFrameModel = TableModel (ExtractedDU TMFrame)


instance ToCellText (ExtractedDU TMFrame) where
  toCellText Nothing _ = defDisplayCell

  toCellText (Just pkt) ColumnDefinition { _columnNumber = 0 } =
    displayCell (textDisplay (pkt ^. epERT)) alignLeft

  toCellText (Just pkt) ColumnDefinition { _columnNumber = 1 } = displayCell
    (textDisplay (pkt ^. epDU . tmFrameHdr . tmFrameScID))
    alignRight

  toCellText (Just pkt) ColumnDefinition { _columnNumber = 2 } =
    displayCell (textDisplay (pkt ^. epDU . tmFrameHdr . tmFrameVcID)) alignLeft

  toCellText (Just pkt) ColumnDefinition { _columnNumber = 3 } =
    displayCell (textDisplay (pkt ^. epDU . tmFrameHdr . tmFrameVCFC)) alignLeft

  toCellText (Just pkt) ColumnDefinition { _columnNumber = 4 } =
    displayCell (textDisplay (pkt ^. epDU . tmFrameHdr . tmFrameMCFC)) alignLeft

  toCellText (Just pkt) ColumnDefinition { _columnNumber = 5 } = displayCell
    (if pkt ^. epDU . tmFrameHdr . tmFrameDfh then "Y" else "N")
    alignLeft

  toCellText (Just pkt) ColumnDefinition { _columnNumber = 6 } =
    displayCell (textDisplay (pkt ^. epSource)) alignLeft

  toCellText (Just pkt) ColumnDefinition { _columnNumber = 7 } =
    case pkt ^. epGap of
      Nothing -> defDisplayCell
      Just (low, _high) ->
        DisplayCell (textDisplay low) alignRight mcsYellow mcsBlack

  toCellText (Just pkt) ColumnDefinition { _columnNumber = 8 } =
    displayCell (textDisplay (pkt ^. epQuality)) alignLeft

  toCellText _ _ = defDisplayCell

