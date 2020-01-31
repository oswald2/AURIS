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
  , epERT
  , epVCID
  )
where


import           RIO

import           Control.Lens                   ( makeLenses )

import           Codec.Serialise
import           Conduit.PayloadParser

import           Data.Aeson
import           General.PUSTypes

import           General.Time

import           Protocol.ProtocolInterfaces



-- | General type for an extracted data unit (DU). Contains metadata
-- about the extracted packet, as the earth reception time (ERT), if
-- there are gaps and so on. The '_epDU' field is the contained packet 
-- itself
data ExtractedDU a = ExtractedDU {
    _epQuality :: Flag Good
    , _epERT :: !SunTime
    , _epGap ::Maybe (Word32, Word32)
    , _epSource :: !ProtocolInterface
    , _epVCID :: !VCID
    , _epDU :: a
} deriving (Eq, Show, Generic)
makeLenses ''ExtractedDU

instance Serialise a => Serialise (ExtractedDU a)
instance FromJSON a => FromJSON (ExtractedDU a)
instance ToJSON a => ToJSON (ExtractedDU a) where
  toEncoding = genericToEncoding defaultOptions

instance GetPayload a => GetPayload (ExtractedDU a) where
  getPayload edu = getPayload (edu ^. epDU)

instance Display a => Display (ExtractedDU a) where 
  -- TODO
  display ExtractedDU {..} = 
    display _epDU


