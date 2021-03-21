{-|
Module      : Data.TM.Parameter
Description : Data type for a parameter value as extracted from a packet
Copyright   : (c) Michael Oswald, 2019
License     : BSD-3
Maintainer  : michael.oswald@onikudaki.net
Stability   : experimental
Portability : POSIX

This module provides a data type which describes a parameter value extracted
from a packet. This is a value, while a parameter definition is the "type" of
the parameter
-}
{-# LANGUAGE
  TemplateHaskell
#-}
module Data.TM.Parameter
  ( TMParameter(..)
  , pName 
  , pTime  
  , pValue 
  , pEngValue

  )
where

import           RIO
import           Control.Lens                   ( makeLenses )
import           Data.Text.Short
import           Codec.Serialise
import           Data.Aeson

import           General.Time
import           Data.TM.Value





-- | Parameter value data type
data TMParameter = TMParameter {
    -- | the name of the parameter
    _pName :: !ShortText
    -- | the timestamp of the paramter value
    , _pTime :: !SunTime
    -- | the value of the parameter
    , _pValue :: TMValue
    -- | the engineering (calibrated) value if applicable
    , _pEngValue :: Maybe TMValue
} deriving (Eq, Show, Generic)
makeLenses ''TMParameter

instance NFData TMParameter
instance Serialise TMParameter
instance FromJSON TMParameter
instance ToJSON TMParameter where
  toEncoding = genericToEncoding defaultOptions
