{-# OPTIONS_GHC -fno-warn-orphans #-}
module Model.TMPParamModel
  ( TMPParamModel
  , ToCellText(..)
  )
where

import           RIO
import qualified Data.Text.Short               as ST
import           Data.TM.Parameter

import           Model.ScrollingTableModel

import           Graphics.UI.FLTK.LowLevel.Fl_Enumerations



type TMPParamModel = TableModel TMParameter


instance ToCellText TMParameter where
  toCellText Nothing _ = DisplayCell "" alignLeft
  toCellText (Just par) ColumnDefinition { _columnNumber = 0 } =
    DisplayCell (ST.toText (par ^. pName)) alignRight
  toCellText (Just par) ColumnDefinition { _columnNumber = 1 } =
    DisplayCell (textDisplay (par ^. pTime)) alignLeft
  toCellText (Just par) ColumnDefinition { _columnNumber = 2 } =
    DisplayCell (textDisplay (par ^. pValue)) alignRight
  toCellText (Just par) ColumnDefinition { _columnNumber = 3 } =
    case par ^. pEngValue of
      Nothing -> DisplayCell "" alignLeft
      Just eng -> DisplayCell (textDisplay eng) alignRight
  toCellText _ _ = DisplayCell "" alignLeft