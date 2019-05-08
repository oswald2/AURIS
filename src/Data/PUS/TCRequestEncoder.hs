{-# LANGUAGE OverloadedStrings
    , BangPatterns
    , GeneralizedNewtypeDeriving
    , DeriveGeneric
    , RecordWildCards
    , TemplateHaskell
#-}
module Data.PUS.TCRequestEncoder
    ( EncodedTCRequest
    , encTcReqPkt
    , encTcReqRqst
    , encodeTCRequest
    , tcRequestEncoderC
    )
where



import           Control.Lens                   ( makeLenses )

import           Data.Conduit

import           Data.PUS.TCPacket
import           Data.PUS.TCRequest



data EncodedTCRequest = EncodedTCRequest {
        _encTcReqPkt :: TCPacket
        , _encTcReqRqst :: TCRequest
    }
    deriving (Eq, Show, Read)

makeLenses ''EncodedTCRequest


encodeTCRequest :: TCRequest -> TCPacket
encodeTCRequest _ = undefined



tcRequestEncoderC :: Monad m => ConduitT TCRequest EncodedTCRequest m ()
tcRequestEncoderC = awaitForever $ \rqst -> do
    let enc = encodeTCRequest rqst
    pure (EncodedTCRequest enc rqst)
