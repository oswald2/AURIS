{-# LANGUAGE
    TemplateHaskell
#-}
module Data.TM.PIVals
  ( TMPIVal(..)
  , tmpiValue
  , tmpiOffset
  , tmpiWidth
  )
where

import           RIO

import           Control.Lens                   ( makeLenses )

import           Codec.Serialise
import           Data.Aeson

import           General.Types


data TMPIVal = TMPIVal {
    _tmpiValue :: !Int64,
    _tmpiOffset :: !ByteOffset,
    _tmpiWidth :: !Word16
    } deriving (Eq, Show, Read, Generic)
makeLenses ''TMPIVal

instance Serialise TMPIVal
instance FromJSON TMPIVal
instance ToJSON TMPIVal where
  toEncoding = genericToEncoding defaultOptions
