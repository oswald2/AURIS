{-# OPTIONS_GHC -fno-warn-orphans #-}
module Model.TMFrameModel
  ( TMFrameModel
  , ToCellText(..)
  )
where

import           RIO
import qualified Data.Text.Short               as ST
import           Data.PUS.TMFrame

import           Model.ScrollingTableModel

import           Graphics.UI.FLTK.LowLevel.Fl_Enumerations



type TMFrameModel = TableModel TMFrame


instance ToCellText TMFrame where
  toCellText Nothing _ = DisplayCell "" alignLeft
  toCellText (Just pkt) ColumnDefinition { _columnNumber = 0 } =
    DisplayCell (textDisplay (pkt ^. tmFrameHdr . tmFrameScID)) alignRight
  toCellText (Just pkt) ColumnDefinition { _columnNumber = 1 } =
    DisplayCell (textDisplay (pkt ^. tmFrameHdr . tmFrameVcID)) alignLeft
  toCellText (Just pkt) ColumnDefinition { _columnNumber = 2 } =
    DisplayCell (textDisplay (pkt ^. tmFrameHdr . tmFrameVCFC)) alignLeft
  toCellText (Just pkt) ColumnDefinition { _columnNumber = 3 } =
    DisplayCell (textDisplay (pkt ^. tmFrameHdr . tmFrameMCFC)) alignLeft
  toCellText (Just pkt) ColumnDefinition { _columnNumber = 4 } =
    DisplayCell (if pkt ^. tmFrameHdr . tmFrameDfh then "Y" else "N") alignLeft
  toCellText _ _ = DisplayCell "" alignLeft