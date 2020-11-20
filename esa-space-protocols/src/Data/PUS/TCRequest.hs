{-# LANGUAGE
    DeriveGeneric
    , NoImplicitPrelude
    , TemplateHaskell
    , RecordWildCards
#-}
module Data.PUS.TCRequest
  ( TCRequest(..)
  , TCRequestBody(..)
  , CommandType(..)
  , tcReqRequestID
  , tcReqMAPID
  , tcReqSCID
  , tcReqVCID
  , tcReqTransMode
  , tcReqPayload
  , tcReqDirective
  , tcReqGetTransmissionMode
  , tcReqTransmissionMode
  , tcReqDestination
  , tcReqPacket
  , tcCmdType
  , _TCCommand
  , _TCDir
  )
where

import           RIO

import           Control.Lens                   ( makeLenses
                                                , makePrisms
                                                )

--import           Data.Binary
import           Data.Aeson
import           Codec.Serialise

import           General.PUSTypes

import           Data.PUS.TCDirective
import           Data.PUS.TCPacket

import           Protocol.ProtocolInterfaces



data CommandType = Space | SCOE 
    deriving (Eq, Ord, Enum, Show, Read, Generic)

instance Serialise CommandType
instance FromJSON CommandType
instance ToJSON CommandType where
  toEncoding = genericToEncoding defaultOptions


data TCRequestBody =
    TCCommand {
        _tcReqMAPID :: MAPID
        , _tcReqTransMode :: TransmissionMode
        , _tcCmdType :: CommandType
        , _tcReqPacket :: TCPacket
        }
    | TCDir {
        _tcReqDirective :: TCDirective
    } deriving (Show, Read, Generic)
makeLenses ''TCRequestBody
makePrisms  ''TCRequestBody


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
    deriving (Show, Read, Generic)
makeLenses ''TCRequest

tcReqGetTransmissionMode :: TCRequest -> TransmissionMode
tcReqGetTransmissionMode req = case _tcReqPayload req of
  TCCommand {..} -> _tcReqTransMode
  TCDir{}        -> AD

tcReqTransmissionMode :: Getting r TCRequest TransmissionMode
tcReqTransmissionMode = to tcReqGetTransmissionMode

instance ProtocolDestination TCRequest where
  destination x = x ^. tcReqDestination


instance Serialise TCRequest
instance FromJSON TCRequest
instance ToJSON TCRequest where
  toEncoding = genericToEncoding defaultOptions



