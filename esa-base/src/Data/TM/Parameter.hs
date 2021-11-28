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
    , eqByName
    , byName
    , byValue
    ) where

import           Codec.Serialise
import           Control.Lens                   ( makeLenses )
import           Data.Aeson
import           Data.Text.Short
import           RIO

import           Data.TM.Value
import           General.Time





-- | Parameter value data type
data TMParameter = TMParameter
    {
    -- | the name of the parameter
      _pName     :: !ShortText
    -- | the timestamp of the paramter value
    , _pTime     :: !SunTime
    -- | the value of the parameter
    , _pValue    :: TMValue
    -- | the engineering (calibrated) value if applicable
    , _pEngValue :: Maybe TMValue
    }
    deriving (Eq, Show, Generic)
makeLenses ''TMParameter

-- | Equality by name only 
eqByName :: TMParameter -> TMParameter -> Bool
eqByName p1 p2 = byName p1 p2 == EQ

-- | Compare parameter values by name
byName :: TMParameter -> TMParameter -> Ordering
byName p1 p2 = compare (p1 ^. pName) (p2 ^. pName)

-- | Compare parameter values by value 
byValue :: TMParameter -> TMParameter -> Ordering
byValue p1 p2 = compare (p1 ^. pValue) (p2 ^. pValue)


instance NFData TMParameter
instance Serialise TMParameter
instance FromJSON TMParameter
instance ToJSON TMParameter where
    toEncoding = genericToEncoding defaultOptions
