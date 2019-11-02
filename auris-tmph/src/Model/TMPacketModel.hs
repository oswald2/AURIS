module Model.TMPacketModel
  ( TMPacketModel
  , ModelValue(..)
  , ToCellText(..)
  )
where

import           RIO
import qualified RIO.Text                      as T
import qualified Data.Text.Short as ST
import           Data.PUS.TMPacket

import           General.Hexdump

import           Model.ScrollingTableModel

import           Graphics.UI.FLTK.LowLevel.FLTKHS
import           Graphics.UI.FLTK.LowLevel.Fl_Enumerations


newtype ModelValue = ModelValue TMPacket 

type TMPacketModel = TableModel ModelValue

instance ToCellText ModelValue where
  toCellText Nothing _ = ("", alignLeft)
  toCellText (Just (ModelValue pkt)) (Column 0) = (textDisplay (pkt ^. tmpSPID), alignRight)
  toCellText (Just (ModelValue pkt)) (Column 1) = (ST.toText (pkt ^. tmpMnemonic), alignLeft)
  toCellText (Just (ModelValue pkt)) (Column 2) = (ST.toText (pkt ^. tmpDescr), alignLeft)
  toCellText (Just (ModelValue pkt)) (Column 3) = (textDisplay (pkt ^. tmpTimeStamp), alignLeft)
  toCellText (Just (ModelValue pkt)) (Column 4) = (textDisplay (pkt ^. tmpERT), alignLeft)
  toCellText (Just (ModelValue pkt)) (Column 5) = (textDisplay (pkt ^. tmpAPID), alignLeft)
  toCellText (Just (ModelValue pkt)) (Column 6) = (textDisplay (pkt ^. tmpType), alignLeft)
  toCellText (Just (ModelValue pkt)) (Column 7) = (textDisplay (pkt ^. tmpSubType), alignLeft)
  toCellText (Just (ModelValue pkt)) (Column 8) = (textDisplay (pkt ^. tmpSSC), alignLeft)
  toCellText (Just (ModelValue pkt)) (Column 9) = (textDisplay (pkt ^. tmpVCID), alignLeft)
