{-# LANGUAGE 
    OverloadedStrings
    , BangPatterns
    , NoImplicitPrelude
    , FlexibleInstances
    , TemplateHaskell
#-}
module Model.PUSPacketModel
  ( PUSPacketModel
  , ModelValue(..)
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

import           Graphics.UI.FLTK.LowLevel.FLTKHS
import           Graphics.UI.FLTK.LowLevel.Fl_Enumerations




newtype ModelValue = ModelValue { getModelValue :: ExtractedDU PUSPacket}

type PUSPacketModel = TableModel ModelValue 


instance ToCellText ModelValue where
  toCellText Nothing _ = ("", alignLeft)
  toCellText (Just (ModelValue pkt)) (Column 0) =
    (T.pack $ maybe "" show (pusPktTime (pkt ^. epDU . pusDfh)), alignLeft)
  toCellText (Just (ModelValue pkt)) (Column 1) = (textDisplay (pkt ^. epERT), alignLeft)
  toCellText (Just (ModelValue pkt)) (Column 2) =
    (textDisplay (pkt ^. epDU . pusHdr . pusHdrTcApid), alignRight)
  toCellText (Just (ModelValue pkt)) (Column 3) =
    (textDisplay (pusType (pkt ^. epDU . pusDfh)), alignRight)
  toCellText (Just (ModelValue pkt)) (Column 4) =
    (textDisplay (pusSubType (pkt ^. epDU . pusDfh)), alignRight)
  toCellText (Just (ModelValue pkt)) (Column 5) =
    (textDisplay (pkt ^. epDU . pusHdr . pusHdrTcSsc), alignRight)
  toCellText (Just (ModelValue pkt)) (Column 6) =
    (hexdumpLineBS (pkt ^. epDU . pusData), alignLeft)
  toCellText (Just _) _ = ("", alignLeft)
