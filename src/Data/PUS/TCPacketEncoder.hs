{-# LANGUAGE OverloadedStrings
    , BangPatterns
    , GeneralizedNewtypeDeriving
    , DeriveGeneric
    , RecordWildCards
    , TemplateHaskell
    , NoImplicitPrelude
#-}
module Data.PUS.TCPacketEncoder
    (
        EncodedTCPacket
        ,encodeTCPacket
        , encTcPUSPacket
        , encTcRequest
        , tcPktEncoderC
    )
where


import RIO

import Control.Lens (makeLenses)    

import Data.Conduit 

import Data.PUS.TCRequest
import Data.PUS.TCRequestEncoder
import Data.PUS.TCPacket
import Data.PUS.PUSPacket


data EncodedTCPacket = EncodedTCPacket {
    _encTcPUSPacket :: PUSPacket
    , _encTcRequest :: TCRequest
}

makeLenses ''EncodedTCPacket

encodeTCPacket :: TCPacket -> PUSPacket
encodeTCPacket _ = undefined 

tcPktEncoderC :: Monad m => ConduitT EncodedTCRequest EncodedTCPacket m ()
tcPktEncoderC = awaitForever $ \request -> do
    let enc = encodeTCPacket (request ^. encTcReqPkt)
    pure (EncodedTCPacket enc (request ^. encTcReqRqst))



