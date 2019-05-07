{-# LANGUAGE
    DeriveGeneric
#-}
module Data.PUS.TCRequest
    (
        TCRequest
    )
where

import GHC.Generics

import Data.Binary
import Data.Aeson




data TCRequest = TCRequest
    deriving (Eq, Show, Read, Generic)

instance Binary TCRequest 

instance FromJSON TCRequest

instance ToJSON TCRequest where
    toEncoding = genericToEncoding defaultOptions



