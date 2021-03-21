{-|
Module      : Data.TM.Validity
Description : Specifies the validity of a value
Copyright   : (c) Michael Oswald, 2019
License     : BSD-3
Maintainer  : michael.oswald@onikudaki.net
Stability   : experimental
Portability : POSIX

This module is for handling the validity of a value
-}
{-# LANGUAGE
    OverloadedStrings
    , BangPatterns
    , NoImplicitPrelude
#-}
module Data.TM.Validity
  ( Validity(..)
  , isValid
  , clearValidity
  , isWrongType
  , validitySetWrongType
  , isOutOfCalibRange
  , validitySetOutOfCalibRange
  , isExtrapolated
  , validitySetExtrapolated
  , isStringNotUtf8
  , setStringNotUtf8
  , isUninitialized
  , setUninitialized
  , isUndefinedValue
  , setUndefinedValue
  )
where

import           RIO
import           Data.Bits
import           Codec.Serialise
import           Data.Aeson


-- | The Validity data type
newtype Validity = Validity { getRawValidity :: Word32 }
    deriving (Eq, Show, Read, Generic)


instance NFData Validity
instance Serialise Validity
instance FromJSON Validity
instance ToJSON Validity where
  toEncoding = genericToEncoding defaultOptions

-- | returns, if the validity is valid
{-# INLINABLE isValid #-}
isValid :: Validity -> Bool
isValid (Validity x) = (x .&. validMask) == 0

{-# INLINABLE validMask #-}
validMask :: Word32
validMask = 0x0FFFFFFF

-- | a default, valid validity
{-# INLINABLE clearValidity #-}
clearValidity :: Validity
clearValidity = Validity 0

{-# INLINABLE wrongType #-}
wrongType :: Word32
wrongType = 0x00000001

{-# INLINABLE outOfCalibRange #-}
outOfCalibRange :: Word32
outOfCalibRange = 0x00000002

{-# INLINABLE extrapolated #-}
extrapolated :: Word32
extrapolated = 0x80000000

{-# INLINABLE stringNotUtf8 #-}
stringNotUtf8 :: Word32
stringNotUtf8 = 0x0000004

{-# INLINABLE uninitialized #-}
uninitialized :: Word32
uninitialized = 0x00000008

{-# INLINABLE undefinedValue #-}
undefinedValue :: Word32
undefinedValue = 0x00000010


-- | returns if the validity has the wrong type bit set
{-# INLINABLE isWrongType #-}
isWrongType :: Validity -> Bool
isWrongType (Validity x) = x .&. wrongType /= 0

-- | sets the wrong type bit
{-# INLINABLE validitySetWrongType #-}
validitySetWrongType :: Validity -> Validity
validitySetWrongType (Validity x) = Validity (x .|. wrongType)

-- | returns if the calibration value was out of range
{-# INLINABLE isOutOfCalibRange #-}
isOutOfCalibRange :: Validity -> Bool
isOutOfCalibRange (Validity x) = x .&. outOfCalibRange /= 0

-- | set the out of range calibration bit
{-# INLINABLE validitySetOutOfCalibRange #-}
validitySetOutOfCalibRange :: Validity -> Validity
validitySetOutOfCalibRange (Validity x) = Validity (x .|. outOfCalibRange)

-- | returns, if the calibration had to be extrapolated
{-# INLINABLE isExtrapolated #-}
isExtrapolated :: Validity -> Bool
isExtrapolated (Validity x) = x .&. extrapolated /= 0

-- | returns, if the passed string was not UTF-8
{-# INLINABLE isStringNotUtf8 #-}
isStringNotUtf8 :: Validity -> Bool
isStringNotUtf8 (Validity x) = x .&. stringNotUtf8 /= 0

-- | sets the "string was not UTF-8" bit
{-# INLINABLE setStringNotUtf8 #-}
setStringNotUtf8 :: Validity -> Validity
setStringNotUtf8 (Validity x) = Validity (x .|. stringNotUtf8)

-- | set the extrapolated flag
{-# INLINABLE validitySetExtrapolated #-}
validitySetExtrapolated :: Validity -> Validity
validitySetExtrapolated (Validity x) = Validity (x .|. extrapolated)

-- | the value is uninitialized
{-# INLINABLE isUninitialized #-}
isUninitialized :: Validity -> Bool
isUninitialized (Validity x) = x .&. uninitialized /= 0

-- | set the uninitialized flag
{-# INLINABLE setUninitialized #-}
setUninitialized :: Validity -> Validity
setUninitialized (Validity x) = Validity (x .|. uninitialized)

-- | the value is undefined
{-# INLINABLE isUndefinedValue #-}
isUndefinedValue :: Validity -> Bool
isUndefinedValue (Validity x) = x .&. undefinedValue /= 0

-- set the undefined flag
{-# INLINABLE setUndefinedValue #-}
setUndefinedValue :: Validity -> Validity
setUndefinedValue (Validity x) = Validity (x .|. undefinedValue)


