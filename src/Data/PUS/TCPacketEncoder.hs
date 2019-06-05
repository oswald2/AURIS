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

import           Control.Lens                   ( makeLenses )

import           Data.Conduit

import           Data.PUS.TCRequest
import           Data.PUS.TCRequestEncoder
import           Data.PUS.TCPacket
import           Data.PUS.PUSPacket




data EncodedTCPacket = EncodedTCPacket {
    _encTcPUSContent :: Maybe PUSPacket
    , _encTcRequest :: TCRequest
}

makeLenses ''EncodedTCPacket

encodeTCPacket :: TCPacket -> PUSPacket
encodeTCPacket _ = undefined

tcPktEncoderC :: Monad m => ConduitT EncodedTCRequest EncodedTCPacket m ()
tcPktEncoderC = awaitForever $ \request -> do
    let req = request ^. encTcReqRqst
    case request ^. encTcReqContent of
        Just tc -> do
            let enc = encodeTCPacket tc
            yield $ EncodedTCPacket (Just enc) req
        Nothing -> yield $ EncodedTCPacket Nothing req



