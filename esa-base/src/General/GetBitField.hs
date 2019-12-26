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
    , getBitFieldInt64
    , getBitFieldWord64
    , GetValue(..)
    , getValueOctet
    , getValueOctetLen
    , MILSingle
    , getMilSingle
    , MILExtended
    , getMilExtended
    , Word48
    , Int48
    , Word24 
    , Int24
    )
where


import           RIO
import qualified RIO.ByteString                as B

import           Data.Bits
import           Data.ReinterpretCast

import           General.Types
import           General.SizeOf

-- | This class is for getting values out of 'ByteString' in case the
-- value is byte-aligned.
class GetValue a where
    getValue :: ByteString -> ByteOffset -> Endian -> Maybe a


instance GetValue Word8 where
    getValue bytes (ByteOffset idx) _ = if idx >= B.length bytes then Nothing else Just $ bytes `B.index` idx
    {-# INLINABLE getValue #-}

instance GetValue Int8 where
    getValue bytes (ByteOffset idx) _ = if idx >= B.length bytes then Nothing else Just . fromIntegral $ bytes `B.index` idx
    {-# INLINABLE getValue #-}

instance GetValue Word16 where
    getValue bytes (ByteOffset idx) byteOrder =
        let !val = case byteOrder of 
              BiE -> fromIntegral (bytes `B.index` idx) `shiftL` 8 .|. fromIntegral
                        (bytes `B.index` (idx + 1))
              LiE -> fromIntegral (bytes `B.index` idx + 1) `shiftL` 8
                    .|. fromIntegral (bytes `B.index` idx) 
        in  
        if idx + 1 >= B.length bytes then Nothing else Just val
    {-# INLINABLE getValue #-}

instance GetValue Int16 where
    getValue bytes off endian =
        fromIntegral <$> getValue @Word16 bytes off endian
    {-# INLINABLE getValue #-}

instance GetValue Word32 where
    getValue bytes (ByteOffset idx) BiE =
        let b0   = fromIntegral (bytes `B.index` idx) `shiftL` 24
            b1   = fromIntegral (bytes `B.index` (idx + 1)) `shiftL` 16
            b2   = fromIntegral (bytes `B.index` (idx + 2)) `shiftL` 8
            b3   = fromIntegral (bytes `B.index` (idx + 3))
            !val = b0 .|. b1 .|. b2 .|. b3
        in if idx + 3 >= B.length bytes then Nothing else Just val
    getValue bytes (ByteOffset idx) LiE =
        let b0   = fromIntegral (bytes `B.index` idx)
            b1   = fromIntegral (bytes `B.index` (idx + 1)) `shiftL` 8
            b2   = fromIntegral (bytes `B.index` (idx + 2)) `shiftL` 16
            b3   = fromIntegral (bytes `B.index` (idx + 3)) `shiftL` 24
            !val = b0 .|. b1 .|. b2 .|. b3
        in if idx + 3 >= B.length bytes then Nothing else Just val
    {-# INLINABLE getValue #-}

instance GetValue Int32 where
    getValue bytes off endian =
        fromIntegral <$> getValue @Word32 bytes off endian
    {-# INLINABLE getValue #-}


newtype Word48 = Word48 { getWord48Val :: Word64 }
    deriving (Eq, Ord, Enum, Num, Real, Integral, Show, Bits, Generic)

newtype Int48 = Int48 { getInt48Val :: Int64 }
    deriving (Eq, Ord, Enum, Num, Real, Integral, Show, Bits, Generic)

newtype Word24 = Word24 { getWord24Val :: Word32 }
    deriving (Eq, Ord, Enum, Num, Real, Integral, Show, Bits, Generic)

newtype Int24 = Int24 { getInt24Val :: Int32 }
    deriving (Eq, Ord, Enum, Num, Real, Integral, Show, Bits, Generic)


instance FixedSize Word24 where 
  fixedSizeOf = 3

instance GetValue Word48 where
  getValue bytes (ByteOffset idx) BiE =
      let b0   = fromIntegral (bytes `B.index` idx) `shiftL` 40
          b1   = fromIntegral (bytes `B.index` (idx + 1)) `shiftL` 32
          b2   = fromIntegral (bytes `B.index` (idx + 2)) `shiftL` 24
          b3   = fromIntegral (bytes `B.index` (idx + 3)) `shiftL` 16
          b4   = fromIntegral (bytes `B.index` (idx + 4)) `shiftL` 8
          b5   = fromIntegral (bytes `B.index` (idx + 5))
          !val = b0 .|. b1 .|. b2 .|. b3 .|. b4 .|. b5
      in if idx + 5 >= B.length bytes then Nothing else Just . Word48 $ val
  getValue bytes (ByteOffset idx) LiE =
      let b0   = fromIntegral (bytes `B.index` idx)
          b1   = fromIntegral (bytes `B.index` (idx + 1)) `shiftL` 8
          b2   = fromIntegral (bytes `B.index` (idx + 2)) `shiftL` 16
          b3   = fromIntegral (bytes `B.index` (idx + 3)) `shiftL` 24
          b4   = fromIntegral (bytes `B.index` (idx + 4)) `shiftL` 32
          b5   = fromIntegral (bytes `B.index` (idx + 5)) `shiftL` 40
          !val = b0 .|. b1 .|. b2 .|. b3 .|. b4 .|. b5
      in if idx + 5 >= B.length bytes then Nothing else Just . Word48 $ val
  {-# INLINABLE getValue #-}

instance GetValue Word24 where 
  getValue bytes (ByteOffset idx) BiE = 
    let b0   = fromIntegral (bytes `B.index` (idx + 3)) `shiftL` 16
        b1   = fromIntegral (bytes `B.index` (idx + 4)) `shiftL` 8
        b2   = fromIntegral (bytes `B.index` (idx + 5))
        !val = b0 .|. b1 .|. b2
    in if idx + 2 >= B.length bytes then Nothing else Just . Word24 $ val
  getValue bytes (ByteOffset idx) LiE =
    let b0   = fromIntegral (bytes `B.index` idx)
        b1   = fromIntegral (bytes `B.index` (idx + 1)) `shiftL` 8
        b2   = fromIntegral (bytes `B.index` (idx + 2)) `shiftL` 16
        !val = b0 .|. b1 .|. b2 
    in if idx + 2 >= B.length bytes then Nothing else Just . Word24 $ val
  {-# INLINABLE getValue #-}



instance GetValue Int48 where
  getValue bytes off endian =
    let val = getWord48Val <$> getValue @Word48 bytes off endian
        !val2 = fmap (\val' -> Int48 . fromIntegral $ if val' .&. 0x00_00_80_00_00_00_00_00 /= 0 then val' .|. 0xFF_FF_00_00_00_00_00_00 else val') val
    in val2

instance GetValue Int24 where 
  getValue bytes off endian = 
    let val = getWord24Val <$> getValue @Word24 bytes off endian 
        !val2 = fmap (\val' -> Int24 . fromIntegral $ if val' .&. 0x00_80_00_00 /= 0 then val' .|. 0xFF_00_00_00 else val') val
    in val2


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
        in if idx + 7 >= B.length bytes then Nothing else Just val
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
        in if idx + 7 >= B.length bytes then Nothing else Just val
    {-# INLINABLE getValue #-}

instance GetValue Int64 where
    getValue bytes off endian = fromIntegral <$> getValue @Word64 bytes off endian
    {-# INLINABLE getValue #-}

instance GetValue Double where
    getValue bytes off endian = wordToDouble <$> getValue bytes off endian
    {-# INLINABLE getValue #-}

instance GetValue Float where
    getValue bytes off endian = wordToFloat <$> getValue bytes off endian
    {-# INLINABLE getValue #-}



newtype MILSingle = MILSingle { getMilSingle :: Double }
  deriving (Show, Generic)

instance GetValue MILSingle where
  getValue bytes off endian = MILSingle . decMILSingle <$> getValue @Word32 bytes off endian

newtype MILExtended = MILExtended { getMilExtended :: Double }
  deriving (Show, Generic)

instance GetValue MILExtended where
  getValue bytes off endian = MILExtended . decMILExtended endian . getWord48Val <$> getValue @Word48 bytes off endian

-- | Get a octet string out of a 'ByteString' with the defined length
getValueOctetLen :: ByteString -> ByteOffset -> Int -> Maybe ByteString
getValueOctetLen bytes (ByteOffset idx) len = if idx + len >= B.length bytes
    then Nothing 
    else Just $ B.take len (B.drop idx bytes)

-- | Get a octet string out of a 'ByteString', taking all bytes until the
-- end of the 'ByteString'. This is used for variable Octet strings and Strings
-- which are at the end of the packet and are consumed till the end
getValueOctet :: ByteString -> ByteOffset -> Maybe ByteString
getValueOctet bytes (ByteOffset idx) = Just $ B.drop idx bytes

{-# INLINABLE byteSwap48 #-}
byteSwap48 :: Word48 -> Word48
byteSwap48 (Word48 w) =
  let b0 = w .&. 0xFF `shiftL` 40
      b1 = (w .&. 0xFF00) `shiftL` 24
      b2 = (w .&. 0xFF0000) `shiftL` 8
      b3 = (w .&. 0xFF000000) `shiftR` 8
      b4 = (w .&. 0xFF00000000) `shiftR` 24
      b5 = (w .&. 0xFF0000000000) `shiftR` 40
      !out = b5
        .|. b4
        .|. b3
        .|. b2
        .|. b1
        .|. b0 `shiftL` 40
  in
    Word48 out

{-# INLINABLE signExtend48 #-}
signExtend48 :: Word48 -> Int48
signExtend48 (Word48 val) =
  let !val2 = if val .&. 0x00_00_80_00_00_00_00_00 /= 0 then val .|. 0xFF_FF_00_00_00_00_00_00 else val in
  Int48 (fromIntegral val2)


{-# INLINABLE signExtend #-}
signExtend :: BitSize -> Word64 -> Int64
signExtend bits w =
  let mask' = 0xFF_FF_FF_FF_FF_FF_FF_FF `shiftL` unBitSize bits
      isNegative = ((1 `shiftL` (unBitSize bits - 1)) .&. w) /= 0
      !newVal = if isNegative then w .|. mask' else w
  in
  fromIntegral newVal


-- | This function gets a 'Int64' out of a 'ByteString' in case the
-- value is not byte-aligned (has a bit-offset)
-- The value has a lenght of 'BitSize', which must be less than 64
{-# INLINABLE getBitFieldInt64 #-}
getBitFieldInt64 :: ByteString -> Offset -> BitSize -> Endian -> Maybe Int64
getBitFieldInt64 bytes off bits BiE = signExtend bits <$> getBitField bytes off bits
getBitFieldInt64 bytes off bits LiE =
  let val = getBitField bytes off bits in
  if | bits == 64 -> fromIntegral . byteSwap64 <$> val
     | bits == 32 -> fromIntegral . byteSwap32 . fromIntegral <$> val
     | bits == 16 -> fromIntegral . byteSwap16 . fromIntegral <$> val 
     | bits == 48 -> fromIntegral . signExtend48 . byteSwap48 . fromIntegral <$> val
     | otherwise -> -- we can't byte-order convert non-byte length values. We
        getBitFieldInt64 bytes off bits BiE

{-# INLINABLE getBitFieldWord64 #-}
getBitFieldWord64 :: ByteString -> Offset -> BitSize -> Endian -> Maybe Word64
getBitFieldWord64 bytes off bits BiE = getBitField bytes off bits
getBitFieldWord64 bytes off bits LiE =
  let val = getBitField bytes off bits in
    if | bits == 64 -> byteSwap64 <$> val
       | bits == 32 -> fromIntegral . byteSwap32 . fromIntegral <$> val
       | bits == 16 -> fromIntegral . byteSwap16 . fromIntegral <$> val
       | bits == 48 -> fromIntegral . byteSwap48 . fromIntegral <$> val
       | otherwise -> -- we can't byte-order convert non-byte length values. We
          getBitFieldWord64 bytes off bits BiE

-- | This function gets a 'Double' out of a 'ByteString' in case the
-- value is not byte-aligned (has a bit-offset)
-- The value has a lenght of 'BitSize', which must be less than 64
{-# INLINABLE getBitFieldDouble #-}
getBitFieldDouble :: ByteString -> Offset -> Endian -> Maybe Double
getBitFieldDouble bytes off BiE = wordToDouble <$> getBitField bytes off (BitSize 64)
getBitFieldDouble bytes off LiE = wordToDouble . byteSwap64 <$> getBitField bytes off (BitSize 64)

{-# INLINABLE getBitFieldFloat #-}
getBitFieldFloat :: ByteString -> Offset -> Endian -> Maybe Float
getBitFieldFloat bytes off BiE = wordToFloat . fromIntegral <$> getBitField bytes off (BitSize 32)
getBitFieldFloat bytes off LiE = wordToFloat . byteSwap32 . fromIntegral <$> getBitField bytes off (BitSize 32)

{-# INLINABLE getBitFieldMilSingle #-}
getBitFieldMilSingle :: ByteString -> Offset -> Endian -> Maybe Double
getBitFieldMilSingle bytes off BiE = decMILSingle . fromIntegral <$> getBitField bytes off (BitSize 32)
getBitFieldMilSingle bytes off LiE = decMILSingle . byteSwap32 . fromIntegral <$> getBitField bytes off (BitSize 32)

{-# INLINABLE getBitFieldMilExtended #-}
getBitFieldMilExtended :: ByteString -> Offset -> Endian -> Maybe Double
getBitFieldMilExtended bytes off endian = decMILExtended endian <$> getBitField bytes off (BitSize 48)



-- | This function gets a 'Word64' out of a 'ByteString' in case the
-- value is not byte-aligned (has a bit-offset).
-- The value has a lenght of 'BitSize', which must be less than 64
{-# INLINABLE getBitField #-}
getBitField :: ByteString -> Offset -> BitSize -> Maybe Word64
getBitField bytes off bs@(BitSize nBits) =
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
    if (unByteOffset . fst . offsetParts . nextByteAligned) (off .+. bs) >= B.length bytes then Nothing else Just value2


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