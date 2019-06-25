{-# LANGUAGE
    DeriveGeneric
    , GeneralizedNewtypeDeriving
#-}
module Data.PUS.TCPacket where


import           Data.Binary
import           Data.Aeson
import           Codec.Serialise

import           GHC.Generics



data TCPacket = TCPacket
    deriving (Eq, Show, Read, Generic)

instance Binary TCPacket
instance Serialise TCPacket
instance FromJSON TCPacket
instance ToJSON TCPacket where
    toEncoding = genericToEncoding defaultOptions


