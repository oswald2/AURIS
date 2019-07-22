{-# LANGUAGE OverloadedStrings
    , BangPatterns
    , GeneralizedNewtypeDeriving
    , DeriveGeneric
    , RecordWildCards
    , TemplateHaskell
    , NoImplicitPrelude
#-}
module Data.PUS.TCPacketEncoder
  ( EncodedTCPacket
  , encodeTCPacket
  , encTcPUSContent
  , encTcRequest
  , tcPktEncoderC
  )
where


import           RIO

import           Control.Lens                   ( makeLenses
                                                , (.~)
                                                )

import           Data.Conduit

import           Data.PUS.TCRequest
import           Data.PUS.TCRequestEncoder
import           Data.PUS.TCPacket
import           Data.PUS.PUSPacket
import           Data.PUS.SegmentationFlags
import           Data.PUS.PUSDfh
import           Data.PUS.Parameter
import           Data.PUS.MissionSpecific.Definitions



data EncodedTCPacket = EncodedTCPacket {
    _encTcPUSContent :: Maybe PUSPacket
    , _encTcRequest :: TCRequest
}
makeLenses ''EncodedTCPacket


encodeTCPacket :: TCPacket -> PUSMissionSpecific -> PUSPacket
encodeTCPacket pkt missionSpecific =
  let hdr = PUSHeader 0 0 PUSTC True (_tcpAPID pkt) SegmentStandalone 0 0 0
      dfh =
          missionSpecific
            ^. pmsTCDataFieldHeader
            &  dfhTypes
            .~ (_tcpType pkt, _tcpSubType pkt)
            &  dfhSourceID
            .~ (_tcpSourceID pkt)
      payload = encodeParameters (_tcpParams pkt)
  in  PUSPacket hdr dfh Nothing payload



tcPktEncoderC :: Monad m => PUSMissionSpecific -> ConduitT EncodedTCRequest EncodedTCPacket m ()
tcPktEncoderC missionSpecific = awaitForever $ \request -> do
  let req = request ^. encTcReqRqst
  case request ^. encTcReqContent of
    Just tc -> do
      let enc = encodeTCPacket tc missionSpecific
      yield $ EncodedTCPacket (Just enc) req
    Nothing -> yield $ EncodedTCPacket Nothing req



