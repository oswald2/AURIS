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
  toCellText Nothing _ = DisplayCell "" alignLeft
  toCellText (Just pkt) ColumnDefinition { _columnNumber = 0 } =
    DisplayCell (T.pack $ maybe "" show (pusPktTime (pkt ^. epDU . pusDfh)))
                alignLeft
  toCellText (Just pkt) ColumnDefinition { _columnNumber = 1 } =
    DisplayCell (textDisplay (pkt ^. epERT)) alignLeft
  toCellText (Just pkt) ColumnDefinition { _columnNumber = 2 } =
    DisplayCell (textDisplay (pkt ^. epDU . pusHdr . pusHdrTcApid)) alignRight
  toCellText (Just pkt) ColumnDefinition { _columnNumber = 3 } =
    DisplayCell (textDisplay (pusType (pkt ^. epDU . pusDfh))) alignRight
  toCellText (Just pkt) ColumnDefinition { _columnNumber = 4 } =
    DisplayCell (textDisplay (pusSubType (pkt ^. epDU . pusDfh))) alignRight
  toCellText (Just pkt) ColumnDefinition { _columnNumber = 5 } =
    DisplayCell (textDisplay (pkt ^. epDU . pusHdr . pusHdrTcSsc)) alignRight
  toCellText (Just pkt) ColumnDefinition { _columnNumber = 6 } =
    DisplayCell (hexdumpLineBS (pkt ^. epDU . pusData)) alignLeft
  toCellText (Just _) _ = DisplayCell "" alignLeft
