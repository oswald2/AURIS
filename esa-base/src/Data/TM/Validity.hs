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
)
where

import RIO
import Data.Bits


newtype Validity = Validity { getRawValidity :: Word32 }
    deriving (Eq, Show, Read)


isValid :: Validity -> Bool 
isValid v@(Validity x) = x == 0 || isExtrapolated v

clearValidity :: Validity 
clearValidity = Validity 0

wrongType :: Word32
wrongType = 0x00000001

outOfCalibRange :: Word32
outOfCalibRange = 0x00000002

extrapolated :: Word32
extrapolated = 0x00000004

isWrongType :: Validity -> Bool
isWrongType (Validity x) = x .&. wrongType /= 0

validitySetWrongType :: Validity -> Validity
validitySetWrongType (Validity x) = Validity (x .|. wrongType)

isOutOfCalibRange :: Validity -> Bool
isOutOfCalibRange (Validity x) = x .&. outOfCalibRange /= 0

validitySetOutOfCalibRange :: Validity -> Validity
validitySetOutOfCalibRange (Validity x) = Validity (x .|. outOfCalibRange)

isExtrapolated :: Validity -> Bool
isExtrapolated (Validity x) = x .&. extrapolated /= 0

validitySetExtrapolated :: Validity -> Validity 
validitySetExtrapolated (Validity x) = Validity (x .|. extrapolated)