{-|
Module      : General.GetBitField
Description : Various functions to extract values from binary data
Copyright   : (c) Michael Oswald, 2019
License     : BSD-3
Maintainer  : michael.oswald@onikudaki.net
Stability   : experimental
Portability : POSIX

This module provides functions to extract values out of binary data. The
values may be on arbitrary bit positions and may have also odd bit widths.
-}
{-# LANGUAGE
    TypeApplications
#-}
module General.GetBitField
    ( getBitField
    , getBitFieldDouble
    , getBitFieldFloat
    , getBitFieldMilSingle
    , getBitFieldMilExtended
    , getBitFieldInt
    , GetValue(..)
    , getValueOctet
    , getValueOctetLen
    , MILSingle
    , getMilSingle
    )
where


import           RIO
import qualified RIO.ByteString                as B

import           Data.Bits
import           Data.ReinterpretCast

import           General.Types


-- | This class is for getting values out of 'ByteString' in case the
-- value is byte-aligned.
class GetValue a where
    getValue :: ByteString -> ByteOffset -> Endian -> a


instance GetValue Word8 where
    getValue bytes (ByteOffset idx) _ = bytes `B.index` idx
    {-# INLINABLE getValue #-}

instance GetValue Int8 where
    getValue bytes (ByteOffset idx) _ = fromIntegral $ bytes `B.index` idx
    {-# INLINABLE getValue #-}

instance GetValue Word16 where
    getValue bytes (ByteOffset idx) BiE =
        let !val =
                    fromIntegral (bytes `B.index` idx) `shiftL` 8 .|. fromIntegral
                        (bytes `B.index` (idx + 1))
        in  val
    getValue bytes (ByteOffset idx) LiE =
        let !val =
                    fromIntegral (bytes `B.index` idx + 1)
                        `shiftL` 8
                        .|.      fromIntegral (bytes `B.index` idx)
        in  val
    {-# INLINABLE getValue #-}

instance GetValue Int16 where
    getValue bytes off endian =
        fromIntegral (getValue bytes off endian :: Word16)
    {-# INLINABLE getValue #-}

instance GetValue Word32 where
    getValue bytes (ByteOffset idx) BiE =
        let b0   = fromIntegral (bytes `B.index` idx) `shiftL` 24
            b1   = fromIntegral (bytes `B.index` (idx + 1)) `shiftL` 16
            b2   = fromIntegral (bytes `B.index` (idx + 2)) `shiftL` 8
            b3   = fromIntegral (bytes `B.index` (idx + 3))
            !val = b0 .|. b1 .|. b2 .|. b3
        in  val
    getValue bytes (ByteOffset idx) LiE =
        let b0   = fromIntegral (bytes `B.index` idx)
            b1   = fromIntegral (bytes `B.index` (idx + 1)) `shiftL` 8
            b2   = fromIntegral (bytes `B.index` (idx + 2)) `shiftL` 16
            b3   = fromIntegral (bytes `B.index` (idx + 3)) `shiftL` 24
            !val = b0 .|. b1 .|. b2 .|. b3
        in  val
    {-# INLINABLE getValue #-}

instance GetValue Int32 where
    getValue bytes off endian =
        fromIntegral (getValue bytes off endian :: Word32)
    {-# INLINABLE getValue #-}


instance GetValue Word64 where
    getValue bytes (ByteOffset idx) BiE =
        let b0   = fromIntegral (bytes `B.index` idx) `shiftL` 56
            b1   = fromIntegral (bytes `B.index` (idx + 1)) `shiftL` 48
            b2   = fromIntegral (bytes `B.index` (idx + 2)) `shiftL` 40
            b3   = fromIntegral (bytes `B.index` (idx + 3)) `shiftL` 32
            b4   = fromIntegral (bytes `B.index` (idx + 4)) `shiftL` 24
            b5   = fromIntegral (bytes `B.index` (idx + 5)) `shiftL` 16
            b6   = fromIntegral (bytes `B.index` (idx + 6)) `shiftL` 8
            b7   = fromIntegral (bytes `B.index` (idx + 7))
            !val = b0 .|. b1 .|. b2 .|. b3 .|. b4 .|. b5 .|. b6 .|. b7
        in  val
    getValue bytes (ByteOffset idx) LiE =
        let b0   = fromIntegral (bytes `B.index` idx)
            b1   = fromIntegral (bytes `B.index` (idx + 1)) `shiftL` 8
            b2   = fromIntegral (bytes `B.index` (idx + 2)) `shiftL` 16
            b3   = fromIntegral (bytes `B.index` (idx + 3)) `shiftL` 24
            b4   = fromIntegral (bytes `B.index` (idx + 4)) `shiftL` 32
            b5   = fromIntegral (bytes `B.index` (idx + 5)) `shiftL` 40
            b6   = fromIntegral (bytes `B.index` (idx + 6)) `shiftL` 48
            b7   = fromIntegral (bytes `B.index` (idx + 7)) `shiftL` 56
            !val = b0 .|. b1 .|. b2 .|. b3 .|. b4 .|. b5 .|. b6 .|. b7
        in  val
    {-# INLINABLE getValue #-}

instance GetValue Int64 where
    getValue bytes off endian = fromIntegral (getValue bytes off endian :: Word64)
    {-# INLINABLE getValue #-}

instance GetValue Double where
    getValue bytes off endian = wordToDouble (getValue bytes off endian)
    {-# INLINABLE getValue #-}

instance GetValue Float where
    getValue bytes off endian = wordToFloat (getValue bytes off endian)
    {-# INLINABLE getValue #-}

newtype Word48 = Word48 { getWord48Val :: Word64 }
  deriving (Show, Generic)


newtype MILSingle = MILSingle { getMilSingle :: Double }
  deriving (Show, Generic)

instance GetValue MILSingle where
  getValue bytes off endian = MILSingle $ decMILSingle $ getValue @Word32 bytes off endian

newtype MILExtended = MILExtended { getMilExtended :: Double }
  deriving (Show, Generic)

instance GetValue MILExtended where
  getValue bytes off endian = MILExtended $ decMILSingle $ getWord48Val $ getValue @Word48 bytes off endian

-- | Get a octet string out of a 'ByteString' with the defined length
getValueOctetLen :: ByteString -> ByteOffset -> Int -> ByteString
getValueOctetLen bytes (ByteOffset idx) len = B.take len (B.drop idx bytes)

-- | Get a octet string out of a 'ByteString', taking all bytes until the
-- end of the 'ByteString'. This is used for variable Octet strings and Strings
-- which are at the end of the packet and are consumed till the end
getValueOctet :: ByteString -> ByteOffset -> ByteString
getValueOctet bytes (ByteOffset idx) = B.drop idx bytes



-- | This function gets a 'Int64' out of a 'ByteString' in case the
-- value is not byte-aligned (has a bit-offset)
-- The value has a lenght of 'BitSize', which must be less than 64
{-# INLINABLE getBitFieldInt #-}
getBitFieldInt :: ByteString -> Offset -> BitSize -> Endian -> Int64
getBitFieldInt bytes off bits endian = fromIntegral (getBitField bytes off bits endian)

-- | This function gets a 'Double' out of a 'ByteString' in case the
-- value is not byte-aligned (has a bit-offset)
-- The value has a lenght of 'BitSize', which must be less than 64
{-# INLINABLE getBitFieldDouble #-}
getBitFieldDouble :: ByteString -> Offset -> Endian -> Double
getBitFieldDouble bytes off endian = wordToDouble (getBitField bytes off (BitSize 64) endian)

{-# INLINABLE getBitFieldFloat #-}
getBitFieldFloat :: ByteString -> Offset -> Endian -> Float
getBitFieldFloat bytes off endian = wordToFloat $ fromIntegral $ getBitField bytes off (BitSize 32) endian

{-# INLINABLE getBitFieldMilSingle #-}
getBitFieldMilSingle :: ByteString -> Offset -> Endian -> Double
getBitFieldMilSingle bytes off endian = decMILSingle $ fromIntegral $ getBitField bytes off (BitSize 32) endian

{-# INLINABLE getBitFieldMilExtended #-}
getBitFieldMilExtended :: ByteString -> Offset -> Endian -> Double
getBitFieldMilExtended bytes off endian = decMILExtended endian $ getBitField bytes off (BitSize 48) endian



-- | This function gets a 'Word64' out of a 'ByteString' in case the
-- value is not byte-aligned (has a bit-offset).
-- The value has a lenght of 'BitSize', which must be less than 64
{-# INLINABLE getBitField #-}
getBitField :: ByteString -> Offset -> BitSize -> Endian -> Word64
getBitField bytes off (BitSize nBits) endian =
    let
        (ByteOffset idx, BitOffset bitNr) = offsetParts off
        value1 :: Word64
        !value1 = fromIntegral $ bytes `B.index` idx .&. (255 `shiftR` bitNr)

        sum1    = bitNr + nBits

        loop1 :: Int -> Word64 -> Int -> (Int, Word64, Int)
        loop1 !nb !val !ix
            | nb >= 8
            = let val2 = (val `shiftL` 8) .|. fromIntegral (bytes `B.index` ix)
              in  loop1 (nb - 8) val2 (ix + 1)
            | otherwise
            = (nb, val, ix + 1)
        !value2 = if sum1 < 8
            then value1 `shiftR` (8 - sum1)
            else
                let (!nb, !val, !ix) =
                        loop1 (nBits - (8 - bitNr)) value1 (idx + 1)
                in
                    if nb > 0
                        then
                            (val `shiftL` nb) .|. fromIntegral
                                ((bytes `B.index` ix) `shiftR` (8 - nb))
                        else val
    in
        -- TODO: check endian of non-64 bit values, this is probably not correct
        case endian of
            BiE -> value2
            LiE -> byteSwap64 value2



{-# INLINABLE decMILSingle #-}
decMILSingle :: Word32 -> Double
decMILSingle b =
    let
        signMask :: Word32
        !signMask = 0x80000000
        exponent1 :: Word32
        !exponent1            = b .&. 0xFF
        (exp1, signExponent) = if signMask .&. exponent1 /= 0
            then (complement exponent1 + 1, 1 :: Int)
            else (exponent1, 0)
        !exp'      = exp1 .&. 0x000000FF
        exp_256_3 = 16777216
        (!mant, !exp'') =
            let mant1 :: Word32
                mant1 = b .&. 0xFFFFFF00 `shiftR` 8
            in  (mant1, exp')
        (!mant', !signMantissa) = if signMask .&. b /= 0
            then (complement mant + 1, 1 :: Int)
            else (mant, 0)
        !mant'' = mant' .&. 0x00FFFFFF
        mantissa :: Double
        !mantissa = fromIntegral mant'' / exp_256_3 * 2
        base :: Double
        !base = mantissa * if signMantissa == 0 then 1 else -1
        !expd = fromIntegral exp''
            * if signExponent == 0 then 1 :: Int else -1

        !result = base * 2 ^^ expd
    in
        result


{-# INLINABLE decMILExtended #-}
decMILExtended :: Endian -> Word64 -> Double
decMILExtended bo b' =
    let !b = case bo of
            BiE -> b'
            LiE -> byteSwap64 b' `shiftR` 16
        mantissaBits :: Word64
        !mantissaBits =
                  ((b .&. 0x00_00_FF_FF_FF_00_00_00) `shiftL` 16)
                  .|. ((b .&. 0x00_00_00_00_00_00_FF_FF) `shiftL` 24)
        mant :: Int64
        mant = fromIntegral mantissaBits
        !mantissa = fromIntegral (mant `div` (1 `shiftL` 24)) / 549755813888.0

        !exponentBits = (b .&. 0x00_00_00_00_00_FF_00_00) `shiftR` 16
        exp1 :: Int8
        exp1      = fromIntegral exponentBits
        exponent1 :: Int
        exponent1 = fromIntegral exp1
        !result   = mantissa * 2 ^^ exponent1
    in
    result