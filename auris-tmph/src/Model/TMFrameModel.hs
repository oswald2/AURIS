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



type TMFrameModel = TableModel (ExtractedDU TMFrame)


instance ToCellText (ExtractedDU TMFrame) where
  toCellText Nothing _ = DisplayCell "" alignLeft
  toCellText (Just pkt) ColumnDefinition { _columnNumber = 0 } =
    DisplayCell (textDisplay (pkt ^. epERT)) alignLeft
  toCellText (Just pkt) ColumnDefinition { _columnNumber = 1 } = DisplayCell
    (textDisplay (pkt ^. epDU . tmFrameHdr . tmFrameScID))
    alignRight
  toCellText (Just pkt) ColumnDefinition { _columnNumber = 2 } =
    DisplayCell (textDisplay (pkt ^. epDU . tmFrameHdr . tmFrameVcID)) alignLeft
  toCellText (Just pkt) ColumnDefinition { _columnNumber = 3 } =
    DisplayCell (textDisplay (pkt ^. epDU . tmFrameHdr . tmFrameVCFC)) alignLeft
  toCellText (Just pkt) ColumnDefinition { _columnNumber = 4 } =
    DisplayCell (textDisplay (pkt ^. epDU . tmFrameHdr . tmFrameMCFC)) alignLeft
  toCellText (Just pkt) ColumnDefinition { _columnNumber = 5 } = DisplayCell
    (if pkt ^. epDU . tmFrameHdr . tmFrameDfh then "Y" else "N")
    alignLeft
  toCellText _ _ = DisplayCell "" alignLeft
