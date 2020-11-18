{-# LANGUAGE 
    TemplateHaskell
#-}
module Data.PUS.TCRequestEncoder
  ( EncodedTCRequest(..)
  , encTcReqContent
  , encTcReqRqst
  , encodeTCRequest
  , tcRequestEncoderC
  )
where


import           RIO
import           Control.Lens                   ( makeLenses )

import           Data.Conduit

import           Data.PUS.TCPacket
import           Data.PUS.TCRequest
import           Data.PUS.Parameter
--import           Data.PUS.TCDirective


data EncodedTCRequest = EncodedTCRequest {
        _encTcReqContent :: Maybe TCPacket 
        , _encTcReqRqst :: !TCRequest
    }
    deriving (Show, Read)
makeLenses ''EncodedTCRequest


encodeTCRequest :: TCRequest -> EncodedTCRequest
encodeTCRequest rqst@TCRequest {_tcReqPayload = TCDir _ } =
  EncodedTCRequest { _encTcReqContent = Nothing, 
    _encTcReqRqst = rqst
  }
encodeTCRequest rqst@TCRequest {_tcReqPayload = TCCommand {..} } =
  let tcPkt = TCPacket { 
        _tcpAPID = _tcReqAPID 
        , _tcpType = _tcReqType 
        , _tcpSubType = _tcReqSubType 
        , _tcpSourceID = _tcReqSourceID 
        , _tcpParams = toSizedParamList _tcReqParameters
        }
  in 
  EncodedTCRequest { _encTcReqContent = Just tcPkt 
    , _encTcReqRqst = rqst 
    }


tcRequestEncoderC :: Monad m => ConduitT TCRequest EncodedTCRequest m ()
tcRequestEncoderC = awaitForever $ \rqst -> pure (encodeTCRequest rqst)
