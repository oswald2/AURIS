{-# LANGUAGE OverloadedStrings
    , BangPatterns
    , GeneralizedNewtypeDeriving
    , DeriveGeneric
    , RecordWildCards
    , TemplateHaskell
#-}
module Data.PUS.TCRequestEncoder
    ( EncodedTCRequest
    , encTcReqContent
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
        _encTcReqContent :: Maybe TCPacket
        , _encTcReqRqst :: TCRequest
    }
    deriving (Eq, Show, Read)

makeLenses ''EncodedTCRequest


encodeTCRequest :: TCRequest -> EncodedTCRequest
encodeTCRequest _ = undefined



tcRequestEncoderC :: Monad m => ConduitT TCRequest EncodedTCRequest m ()
tcRequestEncoderC = awaitForever $ \rqst -> do
    let enc = encodeTCRequest rqst
    pure enc
