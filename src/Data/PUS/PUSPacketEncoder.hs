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
        , pusPktEncoderC
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




pusPktEncoderC :: Monad m => ConduitT EncodedTCPacket EncodedPUSPacket m ()
pusPktEncoderC = awaitForever $ \encTC -> do
    let enc = encodePUSPacket (encTC ^. encTcPUSPacket)
    pure (EncodedPUSPacket enc (encTC ^. encTcRequest))


