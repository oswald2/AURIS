{-# LANGUAGE OverloadedStrings
    , BangPatterns
    , GeneralizedNewtypeDeriving
    , DeriveGeneric
    , RecordWildCards
    , NoImplicitPrelude
    , TemplateHaskell
#-}
module Protocol.ProtocolInterfaces
    ( ProtocolInterface(..)
    , SleIf(..)
    , ProtocolPacket(..)
    , ProtocolDestination(..)
    , ConnectionState(..)
    , ConnType(..)
    , protInterface
    , protContent
    , isNctrs
    , isCnc
    , isEden
    , isSLE
    ) where

import           RIO

import           Control.Lens                   ( makeLenses )

import           Codec.Serialise
import           Data.Aeson
import           Data.Binary


-- | The state of a protocol socket
data ConnectionState =
  Accepting
  | Connected
  | Disconnected
  deriving (Eq, Ord, Enum, Show, Read, Generic)

instance Serialise ConnectionState
instance FromJSON ConnectionState
instance ToJSON ConnectionState where
    toEncoding = genericToEncoding defaultOptions



data ConnType = ConnTC | ConnTM | ConnAdmin | ConnSingle
  deriving (Eq, Ord, Enum, Show, Generic)

instance Hashable ConnType
instance Serialise ConnType
instance FromJSON ConnType
instance ToJSON ConnType where
    toEncoding = genericToEncoding defaultOptions

data SleIf =
    SleRAFIf !Word8
    | SleRCFIf !Word8
    | SleFCLTUIf !Word8
    | SleUnknownIf
    deriving (Eq, Ord, Show, Read ,Generic )

instance Display SleIf where
    display (SleRAFIf   x) = "RAF " <> display x
    display (SleRCFIf   x) = "RCF " <> display x
    display (SleFCLTUIf x) = "FCLTU " <> display x
    display SleUnknownIf   = "UNKNOWN"

instance NFData SleIf
instance Hashable SleIf
instance Binary SleIf
instance Serialise SleIf
instance FromJSON SleIf
instance ToJSON SleIf where
    toEncoding = genericToEncoding defaultOptions


-- | Enumeration of the available interfaces
data ProtocolInterface =
    IfNctrs Word16
    | IfCnc Word16
    | IfEden Word16
    | IfSle SleIf
    deriving (Eq, Ord, Show, Read, Generic)


instance NFData ProtocolInterface
instance Hashable ProtocolInterface
instance Binary ProtocolInterface
instance Serialise ProtocolInterface
instance FromJSON ProtocolInterface
instance ToJSON ProtocolInterface where
    toEncoding = genericToEncoding defaultOptions


instance Display ProtocolInterface where
    display (IfNctrs x) = display @Text "NCTRS " <> display x
    display (IfCnc   x) = display @Text "C&C " <> display x
    display (IfEden  x) = display @Text "EDEN " <> display x
    display (IfSle   x) = display @Text "SLE " <> display x

isNctrs :: ProtocolInterface -> Bool
isNctrs (IfNctrs _) = True
isNctrs _           = False

isCnc :: ProtocolInterface -> Bool
isCnc (IfCnc _) = True
isCnc _         = False

isEden :: ProtocolInterface -> Bool
isEden (IfEden _) = True
isEden _          = False

isSLE :: ProtocolInterface -> Bool
isSLE (IfSle _) = True
isSLE _         = False

-- | This is a simple data type wrapper around another
-- type. It just adds a field with a 'ProtocolInterface' value to specify
-- from which/to which interface the contained packet belongs.
data ProtocolPacket a = ProtocolPacket
    { _protInterface :: ProtocolInterface
    , _protContent   :: a
    }
    deriving Show
makeLenses ''ProtocolPacket


-- | This class is to determine for a certain datatype
-- to which destination it is sent
class ProtocolDestination a where
    destination :: a -> ProtocolInterface
