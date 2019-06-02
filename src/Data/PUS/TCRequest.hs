{-# LANGUAGE
    DeriveGeneric
    , NoImplicitPrelude
    , TemplateHaskell
    , RecordWildCards
#-}
module Data.PUS.TCRequest
    ( TCRequest(..)
    , TCRequestBody(..)
    , tcReqRequestID
    , tcReqMAPID
    , tcReqSCID
    , tcReqVCID
    , tcReqTransMode
    , tcReqPayload
    , tcReqDirective
    , tcReqTransmissionMode
    , tcReqTransmissionModeGetter
    )
where

import           RIO

import           Control.Lens                   ( makeLenses
                                                )

import           Data.Binary
import           Data.Aeson

import           Data.PUS.Types
import           Data.PUS.TCDirective



data TCRequestBody =
    TCCommand {
        _tcReqMAPID :: MAPID
        , _tcReqTransMode :: TransmissionMode
        }
    | TCDir {
        _tcReqDirective :: TCDirective
    } deriving (Eq, Show, Read, Generic)
makeLenses ''TCRequestBody

instance Binary TCRequestBody
instance FromJSON TCRequestBody
instance ToJSON TCRequestBody where
    toEncoding = genericToEncoding defaultOptions

data TCRequest = TCRequest {
    _tcReqRequestID :: RequestID
    , _tcReqSCID :: SCID
    , _tcReqVCID :: VCID
    , _tcReqPayload :: TCRequestBody
    }
    deriving (Eq, Show, Read, Generic)
makeLenses ''TCRequest

tcReqTransmissionMode :: TCRequest -> TransmissionMode
tcReqTransmissionMode req =
    case _tcReqPayload req of
        TCCommand {..} -> _tcReqTransMode
        TCDir{}        -> AD

tcReqTransmissionModeGetter :: Getting r TCRequest TransmissionMode
tcReqTransmissionModeGetter = to tcReqTransmissionMode


instance Binary TCRequest
instance FromJSON TCRequest
instance ToJSON TCRequest where
    toEncoding = genericToEncoding defaultOptions



