{-# LANGUAGE
    OverloadedStrings
    , BangPatterns
    , NoImplicitPrelude
#-}
module Data.TM.Parameter
    ( TMParameter(..)
    )
where

import           RIO
import           Data.Text.Short
import           Codec.Serialise
import           Data.Aeson

import           General.Time
import           Data.TM.Value




data TMParameter = TMParameter {
    _pName :: !ShortText
    , _pTime :: !SunTime
    , _pValue :: TMValue
    , _pEngValue :: Maybe TMValue
} deriving (Show, Generic)

instance NFData TMParameter
instance Serialise TMParameter
instance FromJSON TMParameter
instance ToJSON TMParameter where
  toEncoding = genericToEncoding defaultOptions