{-|
Module      : Data.TM.PIVals
Description : Data type for handling the PI1 and PI2 values for packet identification
Copyright   : (c) Michael Oswald, 2019
License     : BSD-3
Maintainer  : michael.oswald@onikudaki.net
Stability   : experimental
Portability : POSIX

This module provides a data type which describes the PI1 and PI2 values for identifying
a packet. A packet is identified by the tuple (APID, Type, SubType, PI1, PI2).
-}
{-# LANGUAGE
    TemplateHaskell
#-}
module Data.TM.PIVals
    ( TMPIVal(..)
    , tmpiValue
    , tmpiOffset
    , tmpiWidth
    , TMPIValues(..)
    , tmpiP1
    , tmpiP2
    , TMPIDef(..)
    , TMPIDefs(..)
    , tmpidOffset
    , tmpidWidth
    , tmpidP1
    , tmpidP2

    )
where

import           RIO

import           Control.Lens                   ( makeLenses )

import           Codec.Serialise
import           Data.Aeson

import           General.Types


-- | Specifies a PIx value. This value specifies and offset
-- into a packet and a width and a value.
data TMPIVal = TMPIVal {
    -- | the value which should be checked against
    _tmpiValue :: !Int64,
    -- | the offset into the packet where to find the value
    _tmpiOffset :: !ByteOffset,
    -- | the width of the value in bits
    _tmpiWidth :: !BitSize
    } deriving (Eq, Show, Read, Generic)
makeLenses ''TMPIVal

instance Serialise TMPIVal
instance FromJSON TMPIVal
instance ToJSON TMPIVal where
    toEncoding = genericToEncoding defaultOptions

data TMPIDef = TMPIDef {
  _tmpidOffset :: !ByteOffset
  , _tmpidWidth :: !BitSize
  } deriving (Eq, Show, Read, Generic)
makeLenses ''TMPIDef

data TMPIValues = TMPIValues {
  _tmpiP1 :: Maybe TMPIVal
  , _tmpiP2 :: Maybe TMPIVal
  } deriving (Eq, Show, Read, Generic)
makeLenses ''TMPIValues


instance Serialise TMPIValues
instance FromJSON TMPIValues
instance ToJSON TMPIValues where
    toEncoding = genericToEncoding defaultOptions


data TMPIDefs = TMPIDefs {
    _tmpidP1 :: Maybe TMPIDef
    , _tmpidP2 :: Maybe TMPIDef
  } deriving (Eq, Show, Read, Generic)
makeLenses ''TMPIDefs

