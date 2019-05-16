{-# LANGUAGE OverloadedStrings
    , BangPatterns
    , GeneralizedNewtypeDeriving
    , DeriveGeneric
    , RecordWildCards
    , NoImplicitPrelude
    , TemplateHaskell
#-}
module Data.PUS.PUSPacketEncoder
    (
        EncodedPUSPacket
        , encPktEncoded
        , encPktRequest
        , pusPacketEncoderC
        , tcPktToEncPUSC
    )
where


import           RIO

import           Control.Lens                   ( makeLenses )

import           Data.Conduit

import           Data.PUS.PUSPacket
--import           Data.PUS.Segment
import           Data.PUS.TCPacketEncoder
import           Data.PUS.TCRequest


data EncodedPUSPacket = EncodedPUSPacket {
     _encPktEncoded :: ByteString
    , _encPktRequest :: TCRequest
    }

makeLenses ''EncodedPUSPacket



pusPacketEncoderC :: Monad m => ConduitT (PUSPacket, TCRequest) EncodedPUSPacket m ()
pusPacketEncoderC = awaitForever $ \(pkt, rqst) -> do
    let enc = encodePUSPacket pkt
    yield (EncodedPUSPacket enc rqst)


tcPktToEncPUSC :: Monad m => ConduitT EncodedTCPacket EncodedPUSPacket m ()
tcPktToEncPUSC = awaitForever $ \encTC -> do
    let enc = encodePUSPacket (encTC ^. encTcPUSPacket)
    yield (EncodedPUSPacket enc (encTC ^. encTcRequest))


