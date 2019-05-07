{-# LANGUAGE 
    DeriveGeneric
    , GeneralizedNewtypeDeriving
#-}
module Data.PUS.PUSPacket
where


import Data.Binary
import Data.Aeson
    
import GHC.Generics



data PUSPacket = PUSPacket
    deriving (Show, Read, Generic)

instance Binary PUSPacket
instance FromJSON PUSPacket
instance ToJSON PUSPacket where
    toEncoding = genericToEncoding defaultOptions
