{-# OPTIONS_GHC -fno-warn-orphans #-}
module Model.TMPacketModel
  ( TMPacketModel
  , ToCellText(..)
  )
where

import           RIO
import qualified Data.Text.Short               as ST
import           Data.PUS.TMPacket

import           Model.ScrollingTableModel

import           Graphics.UI.FLTK.LowLevel.Fl_Enumerations



type TMPacketModel = TableModel TMPacket


instance ToCellText TMPacket where
  toCellText Nothing _ = DisplayCell "" alignLeft
  toCellText (Just pkt) ColumnDefinition { _columnNumber = 0 } =
    DisplayCell (textDisplay (pkt ^. tmpSPID)) alignRight
  toCellText (Just pkt) ColumnDefinition { _columnNumber = 1 } =
    DisplayCell (ST.toText (pkt ^. tmpMnemonic)) alignLeft
  toCellText (Just pkt) ColumnDefinition { _columnNumber = 2 } =
    DisplayCell (ST.toText (pkt ^. tmpDescr)) alignLeft
  toCellText (Just pkt) ColumnDefinition { _columnNumber = 3 } =
    DisplayCell (textDisplay (pkt ^. tmpTimeStamp)) alignLeft
  toCellText (Just pkt) ColumnDefinition { _columnNumber = 4 } =
    DisplayCell (textDisplay (pkt ^. tmpERT)) alignLeft
  toCellText (Just pkt) ColumnDefinition { _columnNumber = 5 } =
    DisplayCell (textDisplay (pkt ^. tmpAPID)) alignLeft
  toCellText (Just pkt) ColumnDefinition { _columnNumber = 6 } =
    DisplayCell (textDisplay (pkt ^. tmpType)) alignLeft
  toCellText (Just pkt) ColumnDefinition { _columnNumber = 7 } =
    DisplayCell (textDisplay (pkt ^. tmpSubType)) alignLeft
  toCellText (Just pkt) ColumnDefinition { _columnNumber = 8 } =
    DisplayCell (textDisplay (pkt ^. tmpSSC)) alignLeft
  toCellText (Just pkt) ColumnDefinition { _columnNumber = 9 } =
    DisplayCell (textDisplay (pkt ^. tmpVCID)) alignLeft
  toCellText _ _ = DisplayCell "" alignLeft