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
    , tcReqDestination
    )
where

import           RIO

import           Control.Lens                   ( makeLenses )

import           Data.Binary
import           Data.Aeson
import           Codec.Serialise

import           General.PUSTypes
import           Data.PUS.TCDirective

import           Protocol.ProtocolInterfaces



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
instance Serialise TCRequestBody
instance FromJSON TCRequestBody
instance ToJSON TCRequestBody where
    toEncoding = genericToEncoding defaultOptions

data TCRequest = TCRequest {
    _tcReqRequestID :: RequestID
    , _tcReqDestination :: ProtocolInterface
    , _tcReqSCID :: SCID
    , _tcReqVCID :: VCID
    , _tcReqPayload :: TCRequestBody
    }
    deriving (Eq, Show, Read, Generic)
makeLenses ''TCRequest

tcReqTransmissionMode :: TCRequest -> TransmissionMode
tcReqTransmissionMode req = case _tcReqPayload req of
    TCCommand {..} -> _tcReqTransMode
    TCDir{}        -> AD

tcReqTransmissionModeGetter :: Getting r TCRequest TransmissionMode
tcReqTransmissionModeGetter = to tcReqTransmissionMode

instance ProtocolDestination TCRequest where
    destination x = x ^. tcReqDestination


instance Binary TCRequest
instance Serialise TCRequest
instance FromJSON TCRequest
instance ToJSON TCRequest where
    toEncoding = genericToEncoding defaultOptions



