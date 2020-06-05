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



type TMPParamModel = VectorTableModel TMParameter


instance ToCellText TMParameter where
  toCellText par ColumnDefinition { _columnNumber = 0 } =
    displayCell (ST.toText (par ^. pName)) alignRight
  toCellText par ColumnDefinition { _columnNumber = 1 } =
    displayCell (textDisplay (par ^. pTime)) alignLeft
  toCellText par ColumnDefinition { _columnNumber = 2 } =
    displayCell (textDisplay (par ^. pValue)) alignRight
  toCellText _ _ = defDisplayCell