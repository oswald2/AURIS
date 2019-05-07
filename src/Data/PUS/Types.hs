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
    )
where

    
import           Data.Binary
import           Data.Aeson
import           Data.ByteString.Builder
import           Data.Attoparsec.ByteString     ( Parser )
import qualified Data.Attoparsec.ByteString    as A
import qualified Data.Attoparsec.Binary        as A

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
