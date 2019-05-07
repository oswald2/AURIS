{-# LANGUAGE 
    DeriveGeneric
    , GeneralizedNewtypeDeriving
#-}
module Data.PUS.TCPacket
where


import Data.Binary
import Data.Aeson

import GHC.Generics



data TCPacket = TCPacket
    deriving (Show, Read, Generic)

instance Binary TCPacket
instance FromJSON TCPacket
instance ToJSON TCPacket where
    toEncoding = genericToEncoding defaultOptions


