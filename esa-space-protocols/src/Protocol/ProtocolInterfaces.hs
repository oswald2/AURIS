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
        , isNctrs
        , isCnc
        , isEden
        , isEdenScoe
    )
where

import RIO

import Control.Lens (makeLenses)

import Data.Binary
import Data.Aeson
import Codec.Serialise


-- | Enumeration of the available interfaces
data ProtocolInterface =
    IfNctrs Word16
    | IfCnc Word16
    | IfEden Word16
    | IfEdenScoe Word16
    deriving (Eq, Ord, Show, Read, Generic)


instance Hashable ProtocolInterface
instance Binary ProtocolInterface
instance Serialise ProtocolInterface
instance FromJSON ProtocolInterface
instance ToJSON ProtocolInterface where
    toEncoding = genericToEncoding defaultOptions


instance Display ProtocolInterface where
  display (IfNctrs x) = display @Text "NCTRS " <> display x
  display (IfCnc x) = display @Text "C&C " <> display x 
  display (IfEden x) = display @Text "EDEN " <> display x
  display (IfEdenScoe x) = display @Text "EDEN (SCOE) " <> display x


isNctrs :: ProtocolInterface -> Bool 
isNctrs (IfNctrs _) = True 
isNctrs _ = False 

isCnc :: ProtocolInterface -> Bool 
isCnc (IfCnc _) = True 
isCnc _ = False 

isEden :: ProtocolInterface -> Bool 
isEden (IfEden _) = True 
isEden _ = False 

isEdenScoe :: ProtocolInterface -> Bool 
isEdenScoe (IfEdenScoe _) = True 
isEdenScoe _ = False 


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
