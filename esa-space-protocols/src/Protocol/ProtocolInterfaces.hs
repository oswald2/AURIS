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
        , ProtocolDestination(..)
        , protInterface
        , protContent
    )
where

import RIO

import Control.Lens (makeLenses)

import Data.Binary
import Data.Aeson
import Codec.Serialise


-- | Enumeration of the available interfaces
data ProtocolInterface =
    IF_NCTRS
    | IF_CNC
    | IF_EDEN
    | IF_EDEN_SCOE
    deriving (Eq, Ord, Enum, Show, Read, Generic)

instance Binary ProtocolInterface
instance Serialise ProtocolInterface
instance FromJSON ProtocolInterface
instance ToJSON ProtocolInterface where
    toEncoding = genericToEncoding defaultOptions

-- | This is a simple data type wrapper around another
-- type. It just adds a field with a 'ProtocolInterface' value to specify
-- from which/to which interface the contained packet belongs.
data ProtocolPacket a = ProtocolPacket {
    _protInterface :: ProtocolInterface
    , _protContent :: a
    } deriving (Show)
makeLenses ''ProtocolPacket


-- | This class is to determine for a certain datatype
-- to which destination it is sent
class ProtocolDestination a where
    destination :: a -> ProtocolInterface
