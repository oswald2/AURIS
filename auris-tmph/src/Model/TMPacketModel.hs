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
  toCellText Nothing _ = defDisplayCell
  toCellText (Just pkt) ColumnDefinition { _columnNumber = 0 } =
    displayCell (textDisplay (pkt ^. tmpSPID)) alignRight
  toCellText (Just pkt) ColumnDefinition { _columnNumber = 1 } =
    displayCell (ST.toText (pkt ^. tmpMnemonic)) alignLeft
  toCellText (Just pkt) ColumnDefinition { _columnNumber = 2 } =
    displayCell (ST.toText (pkt ^. tmpDescr)) alignLeft
  toCellText (Just pkt) ColumnDefinition { _columnNumber = 3 } =
    displayCell (textDisplay (pkt ^. tmpTimeStamp)) alignLeft
  toCellText (Just pkt) ColumnDefinition { _columnNumber = 4 } =
    displayCell (textDisplay (pkt ^. tmpERT)) alignLeft
  toCellText (Just pkt) ColumnDefinition { _columnNumber = 5 } =
    displayCell (textDisplay (pkt ^. tmpAPID)) alignLeft
  toCellText (Just pkt) ColumnDefinition { _columnNumber = 6 } =
    displayCell (textDisplay (pkt ^. tmpType)) alignLeft
  toCellText (Just pkt) ColumnDefinition { _columnNumber = 7 } =
    displayCell (textDisplay (pkt ^. tmpSubType)) alignLeft
  toCellText (Just pkt) ColumnDefinition { _columnNumber = 8 } =
    displayCell (textDisplay (pkt ^. tmpSSC)) alignLeft
  toCellText (Just pkt) ColumnDefinition { _columnNumber = 9 } =
    displayCell (textDisplay (pkt ^. tmpVCID)) alignLeft
  toCellText _ _ = defDisplayCell