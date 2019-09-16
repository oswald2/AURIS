{-# LANGUAGE 
    OverloadedStrings
    , BangPatterns
    , NoImplicitPrelude
    , FlexibleInstances
#-}
module Model.PUSPacketModel
  ( PUSPacketModel
  , ToCellText(..)
  , addPacketToModel
  , modelMaxRows
  )
where


import           RIO
import qualified RIO.Text                      as T

import qualified Data.Sequence                 as S

import           Data.PUS.PUSPacket
import           Data.PUS.PUSDfh
import           Data.PUS.ExtractedDU

import           General.Hexdump



type PUSPacketModel = Seq (ExtractedDU PUSPacket)


modelMaxRows :: Int
modelMaxRows = 20


addPacketToModel :: ExtractedDU PUSPacket -> PUSPacketModel -> PUSPacketModel
addPacketToModel pkt model =
  let len = S.length model
  in  if len < modelMaxRows then model S.|> pkt else (S.drop 1 model) S.|> pkt



class ToCellText a where
    toCellText :: Maybe a -> Int -> Text


instance ToCellText (ExtractedDU PUSPacket) where
  toCellText Nothing _ = ""
  toCellText (Just pkt) 0 =
    T.pack $ maybe "" show (pusPktTime (pkt ^. epDU . pusDfh))
  toCellText (Just _pkt) 1 = ""
  toCellText (Just pkt ) 2 = textDisplay (pkt ^. epDU . pusHdr . pusHdrTcApid)
  toCellText (Just pkt ) 3 = textDisplay (pusType (pkt ^. epDU . pusDfh))
  toCellText (Just pkt ) 4 = textDisplay (pusSubType (pkt ^. epDU . pusDfh))
  toCellText (Just pkt ) 5 = textDisplay (pkt ^. epDU . pusHdr . pusHdrTcSsc)
  toCellText (Just pkt ) 6 = hexdumpLineBS (pkt ^. epDU . pusData)
  toCellText (Just _   ) _ = ""
