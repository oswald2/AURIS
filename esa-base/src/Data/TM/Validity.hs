{-# LANGUAGE
    OverloadedStrings
    , BangPatterns
    , NoImplicitPrelude
#-}
module Data.TM.Validity
(
    Validity
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

import RIO
import Data.Bits


newtype Validity = Validity { getRawValidity :: Word32 }
    deriving (Eq, Show, Read)

{-# INLINABLE isValid #-}
isValid :: Validity -> Bool
isValid (Validity x) = (x .&. validMask) == 0

{-# INLINABLE validMask #-}
validMask :: Word32
validMask = 0x0FFFFFFF

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


{-# INLINABLE isWrongType #-}
isWrongType :: Validity -> Bool
isWrongType (Validity x) = x .&. wrongType /= 0

{-# INLINABLE validitySetWrongType #-}
validitySetWrongType :: Validity -> Validity
validitySetWrongType (Validity x) = Validity (x .|. wrongType)

{-# INLINABLE isOutOfCalibRange #-}
isOutOfCalibRange :: Validity -> Bool
isOutOfCalibRange (Validity x) = x .&. outOfCalibRange /= 0

{-# INLINABLE validitySetOutOfCalibRange #-}
validitySetOutOfCalibRange :: Validity -> Validity
validitySetOutOfCalibRange (Validity x) = Validity (x .|. outOfCalibRange)

{-# INLINABLE isExtrapolated #-}
isExtrapolated :: Validity -> Bool
isExtrapolated (Validity x) = x .&. extrapolated /= 0

{-# INLINABLE isStringNotUtf8 #-}
isStringNotUtf8 :: Validity -> Bool
isStringNotUtf8 (Validity x) = x .&. stringNotUtf8 /= 0

{-# INLINABLE setStringNotUtf8 #-}
setStringNotUtf8 :: Validity -> Validity
setStringNotUtf8 (Validity x) = Validity (x .|. stringNotUtf8)

{-# INLINABLE validitySetExtrapolated #-}
validitySetExtrapolated :: Validity -> Validity
validitySetExtrapolated (Validity x) = Validity (x .|. extrapolated)

{-# INLINABLE isUninitialized #-}
isUninitialized :: Validity -> Bool
isUninitialized (Validity x) = x .&. uninitialized /= 0

{-# INLINABLE setUninitialized #-}
setUninitialized :: Validity -> Validity
setUninitialized (Validity x) = Validity (x .|. uninitialized)

{-# INLINABLE isUndefinedValue #-}
isUndefinedValue :: Validity -> Bool
isUndefinedValue (Validity x) = x .&. undefinedValue /= 0

{-# INLINABLE setUndefinedValue #-}
setUndefinedValue :: Validity -> Validity
setUndefinedValue (Validity x) = Validity (x .|. undefinedValue)
