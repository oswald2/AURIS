{-# LANGUAGE
    DeriveGeneric
    , GeneralizedNewtypeDeriving
#-}
module Data.PUS.Types
    ( VCID(..)
    , mkVCID
    , vcidBuilder
    , vcidParser
    , SCID(..)
    , mkSCID
    , scidBuilder
    , scidParser
    , MAPID
    , mkMAPID
    , getMAPID
    , mapIDBuilder
    , mapIDParser
    , mapIDControl
    , Flag
    , toFlag
    , fromFlag
    , Ready(..)
    , Enable(..)
    , OnOff(..)
    , RequestID
    , getRqstID
    , mkRqstID
    )
where


import           Data.Binary
import           Data.Aeson
import           ByteString.StrictBuilder
import           Data.Attoparsec.ByteString     ( Parser )
import qualified Data.Attoparsec.ByteString    as A
import qualified Data.Attoparsec.Binary        as A
import           Data.Bits
import           Data.Int

import           GHC.Generics



newtype VCID = VCID { getVCID :: Word8 }
    deriving (Eq, Ord, Num, Show, Read, Generic)

mkVCID :: Word8 -> VCID
mkVCID = VCID

instance Binary VCID
instance FromJSON VCID
instance ToJSON VCID where
    toEncoding = genericToEncoding defaultOptions

vcidBuilder :: VCID -> Builder
vcidBuilder (VCID x) = word8 x

vcidParser :: Parser VCID
vcidParser = VCID <$> A.anyWord8



newtype SCID = SCID { getSCID :: Word16 }
    deriving (Eq, Ord, Show, Read, Generic)

mkSCID :: Word16 -> SCID
mkSCID = SCID

instance Binary SCID
instance FromJSON SCID
instance ToJSON SCID where
    toEncoding = genericToEncoding defaultOptions

scidBuilder :: SCID -> Builder
scidBuilder (SCID x) = word16BE x

scidParser :: Parser SCID
scidParser = SCID <$> A.anyWord16be




newtype MAPID = MAPID { getMAPID :: Word8 }
    deriving (Eq, Ord, Num, Show, Read, Generic)

mkMAPID :: Word8 -> MAPID
mkMAPID x = MAPID (x .&. 0x3F)

instance Binary MAPID
instance FromJSON MAPID
instance ToJSON MAPID where
    toEncoding = genericToEncoding defaultOptions

mapIDBuilder :: MAPID -> Builder
mapIDBuilder (MAPID x) = word8 x

mapIDParser :: Parser MAPID
mapIDParser = MAPID <$> A.anyWord8

mapIDControl :: MAPID
mapIDControl = MAPID 63



data Ready = Ready
data Enable = Enable
data OnOff = OnOff


newtype Flag a = MkFlag Bool
    deriving (Eq, Ord, Show, Read, Generic)

toFlag :: t -> Bool -> Flag t
toFlag _ = MkFlag

fromFlag :: t -> Flag t -> Bool
fromFlag _ (MkFlag b) = b



newtype RequestID = RequestID { getRqstID :: Int64 }
    deriving (Eq, Ord, Num, Show, Read, Generic)

mkRqstID :: Int64 -> RequestID
mkRqstID = RequestID

instance Binary RequestID
instance FromJSON RequestID
instance ToJSON RequestID where
    toEncoding = genericToEncoding defaultOptions



-- | provide