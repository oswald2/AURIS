{-# LANGUAGE
    OverloadedStrings
    , BangPatterns
    , NoImplicitPrelude
    , FlexibleInstances
    , TemplateHaskell
#-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Model.PUSPacketModel
  ( PUSPacketModel
  , ToCellText(..)
  )
where


import           RIO
import qualified RIO.Text                      as T

--import           Control.Lens                   ( makeLenses )

--import qualified Data.Sequence                 as S

import           Data.PUS.PUSPacket
import           Data.PUS.PUSDfh
import           Data.PUS.ExtractedDU

import           General.Hexdump

import           Model.ScrollingTableModel

import           Graphics.UI.FLTK.LowLevel.Fl_Enumerations





type PUSPacketModel = TableModel (ExtractedDU PUSPacket)


instance ToCellText (ExtractedDU PUSPacket) where
  toCellText pkt ColumnDefinition { _columnNumber = 0 } =
    displayCell (T.pack $ maybe "" show (pusPktTime (pkt ^. epDU . pusDfh)))
                alignLeft
  toCellText pkt ColumnDefinition { _columnNumber = 1 } =
    displayCell (textDisplay (pkt ^. epERT)) alignLeft
  toCellText pkt ColumnDefinition { _columnNumber = 2 } =
    displayCell (textDisplay (pkt ^. epDU . pusHdr . pusHdrAPID)) alignRight
  toCellText pkt ColumnDefinition { _columnNumber = 3 } =
    displayCell (textDisplay (pusType (pkt ^. epDU . pusDfh))) alignRight
  toCellText pkt ColumnDefinition { _columnNumber = 4 } =
    displayCell (textDisplay (pusSubType (pkt ^. epDU . pusDfh))) alignRight
  toCellText pkt ColumnDefinition { _columnNumber = 5 } =
    displayCell (textDisplay (pkt ^. epDU . pusHdr . pusHdrSSC)) alignRight
  toCellText pkt ColumnDefinition { _columnNumber = 6 } =
    displayCell (hexdumpLineBS (pkt ^. epDU . pusData)) alignLeft
  toCellText _ _ = defDisplayCell
