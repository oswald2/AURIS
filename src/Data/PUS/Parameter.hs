{-# LANGUAGE 
    OverloadedStrings
    , BangPatterns
    , DeriveGeneric
    , NoImplicitPrelude
#-}
module Data.PUS.Parameter
    (
        Parameter(..)
    )
where


import RIO

import Data.Binary
import Data.Aeson



data Parameter = Parameter
    deriving (Eq, Show, Read, Generic)

instance Binary Parameter
instance FromJSON Parameter
instance ToJSON Parameter where
    toEncoding = genericToEncoding defaultOptions
