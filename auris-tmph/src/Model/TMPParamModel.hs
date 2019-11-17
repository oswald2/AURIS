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
  toCellText Nothing _ = defDisplayCell
  toCellText (Just par) ColumnDefinition { _columnNumber = 0 } =
    displayCell (ST.toText (par ^. pName)) alignRight
  toCellText (Just par) ColumnDefinition { _columnNumber = 1 } =
    displayCell (textDisplay (par ^. pTime)) alignLeft
  toCellText (Just par) ColumnDefinition { _columnNumber = 2 } =
    displayCell (textDisplay (par ^. pValue)) alignRight
  toCellText (Just par) ColumnDefinition { _columnNumber = 3 } =
    case par ^. pEngValue of
      Nothing -> defDisplayCell
      Just eng -> displayCell (textDisplay eng) alignRight
  toCellText _ _ = defDisplayCell