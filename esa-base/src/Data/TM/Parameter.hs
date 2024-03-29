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
    , defaultValue
    , defaultParameterByDef
    ) where

import           Codec.Serialise
import           Control.Lens                   ( makeLenses )
import           Data.Aeson
import           Data.Text.Short
import           RIO

import           Data.TM.TMParameterDef
import           Data.TM.Validity
import           Data.TM.Value
import           General.Time
import           General.Types                  ( HexBytes(..) )



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



defaultValue :: ParamType -> TMValue
defaultValue (ParamInteger  _    ) = TMValue (TMValInt 0) uninitializedValidity
defaultValue (ParamUInteger _    ) = TMValue (TMValUInt 0) uninitializedValidity
defaultValue (ParamDouble   _    ) = TMValue (TMValDouble 0) uninitializedValidity
defaultValue (ParamTime _ _      ) = TMValue (TMValTime nullTime) uninitializedValidity
defaultValue (ParamString  _     ) = TMValue (TMValString "") uninitializedValidity
defaultValue (ParamOctet _) = TMValue (TMValOctet (HexBytes "")) uninitializedValidity
defaultValue (ParamDeduced _     ) = TMValue (TMValNothing) uninitializedValidity
defaultValue (ParamSavedSynthetic) = TMValue (TMValNothing) uninitializedValidity


defaultParameterByDef :: TMParameterDef -> TMParameter
defaultParameterByDef def = TMParameter
    { _pName     = def ^. fpName
    , _pTime     = nullTime
    , _pValue    = defaultValue (def ^. fpType)
    , _pEngValue = Nothing
    }


