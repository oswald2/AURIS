{-# LANGUAGE
    OverloadedStrings
    , BangPatterns
    , NoImplicitPrelude
    , TemplateHaskell
    , DeriveGeneric
#-}
module Data.PUS.ExtractedDU
  ( ExtractedDU(..)
  , epDU
  , epQuality
  , epSource
  , epGap
  )
where


import           RIO

import           Control.Lens                   ( makeLenses )

import           Codec.Serialise
import           Conduit.PayloadParser

import           Data.Aeson
import           Data.PUS.Types

import           General.Time

import           Protocol.ProtocolInterfaces




data ExtractedDU a = ExtractedDU {
    _epQuality :: Flag Good
    , _epERT :: !SunTime
    , _epGap ::Maybe (Word32, Word32)
    , _epSource :: !ProtocolInterface
    , _epDU :: a
} deriving (Eq, Show, Generic)
makeLenses ''ExtractedDU

instance Serialise a => Serialise (ExtractedDU a)
instance FromJSON a => FromJSON (ExtractedDU a)
instance ToJSON a => ToJSON (ExtractedDU a) where
  toEncoding = genericToEncoding defaultOptions

instance GetPayload a => GetPayload (ExtractedDU a) where
  getPayload edu = getPayload (edu ^. epDU)
