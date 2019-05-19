{-# LANGUAGE OverloadedStrings
    , BangPatterns
    , GeneralizedNewtypeDeriving
    , DeriveGeneric
    , RecordWildCards
    , NoImplicitPrelude
    , TemplateHaskell
#-}
module Protocol.ProtocolInterfaces
    (
        ProtocolInterface(..)
        , ProtocolPacket(..)
        , protInterface
        , protContent
    )
where

import RIO

import Control.Lens (makeLenses)

import Data.Binary
import Data.Aeson



data ProtocolInterface =
    IF_NCTRS
    | IF_CNC
    | IF_EDEN
    | IF_EDEN_SCOE
    deriving (Eq, Ord, Enum, Show, Read, Generic)

instance Binary ProtocolInterface
instance FromJSON ProtocolInterface
instance ToJSON ProtocolInterface where
    toEncoding = genericToEncoding defaultOptions


data ProtocolPacket a = ProtocolPacket {
    _protInterface :: ProtocolInterface
    , _protContent :: a
    }
makeLenses ''ProtocolPacket

