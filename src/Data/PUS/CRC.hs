{-|
Module      : Data.PUS.CRC
Description : Provides functions for CRC calculations
Copyright   : (c) Michael Oswald, 2019
License     : BSD-3
Maintainer  : michael.oswald@onikudaki.net
Stability   : experimental
Portability : POSIX

This module is used for calculating and appending a CRC value to 
ByteStrings.
-}
{-# LANGUAGE BangPatterns,
    NoImplicitPrelude
#-}
module Data.PUS.CRC
    ( CRC
    , crcCalc
    , crcCalcBL
    , crcEncode
    , crcEncodeBS
    , crcEncodeBL
    , crcEncodeAndAppendBS
    )
where

import           RIO
import qualified RIO.ByteString                as BS
import qualified RIO.ByteString.Lazy           as BL
import           Data.ByteString.Builder

import           Data.Bits
import qualified Data.Vector.Unboxed           as V


-- | The CRC type
newtype CRC = CRC Word16


-- | Calculates the CRC for the given strict 'ByteString' 
{-# INLINABLE crcCalc #-}
crcCalc :: ByteString -> CRC
crcCalc = CRC . BS.foldl' newst 0xFFFF
  where
    crc :: Word16 -> Word16 -> Word16
    crc !acc !byte = x1 `xor` x3
      where
        !x1 = (((acc `shiftL` 8) .&. 0xFF00))
        !x2 = (acc `shiftR` 8) `xor` byte
        !x3 = crcTable `V.unsafeIndex` fromIntegral x2
    newst acc byte = crc acc (fromIntegral byte)


  
-- | Calculates the CRC for the given lazy 'ByteString' 
{-# INLINABLE crcCalcBL #-}
crcCalcBL :: BL.ByteString -> CRC
crcCalcBL = CRC . BL.foldl' newst 0xFFFF
  where
    crc :: Word16 -> Word16 -> Word16
    crc !acc !byte = x1 `xor` x3
      where
        !x1 = (((acc `shiftL` 8) .&. 0xFF00))
        !x2 = (acc `shiftR` 8) `xor` byte
        !x3 = crcTable `V.unsafeIndex` fromIntegral x2
    newst acc byte = crc acc (fromIntegral byte)


-- | Encodes the CRC to a 'Builder'
{-# INLINABLE crcEncode #-}
crcEncode :: CRC -> Builder
crcEncode (CRC c) = word16BE c

-- | Encodes the CRC to a lazy 'ByteString'
{-# INLINABLE crcEncodeBL #-}
crcEncodeBL :: CRC -> BL.ByteString
crcEncodeBL = toLazyByteString . crcEncode


-- | Encodes the CRC to a strict 'ByteString'
{-# INLINABLE crcEncodeBS #-}
crcEncodeBS :: CRC -> ByteString
crcEncodeBS = BL.toStrict . crcEncodeBL

-- | Calculates the CRC for the given 'ByteString' and
-- appends the CRC
{-# INLINABLE crcEncodeAndAppendBS #-}
crcEncodeAndAppendBS :: ByteString -> ByteString
crcEncodeAndAppendBS bs =
    let c       = crcCalc bs
        !encCrc = crcEncodeBS c
    in  bs <> encCrc


{-# INLINABLE crcTable #-}
crcTable :: V.Vector Word16
crcTable = V.fromList (map (createVal 0 0) [0 .. 255])


-- | Helper for initialising the CRC table
{-# INLINABLE createVal #-}
createVal :: Int -> Word16 -> Word8 -> Word16
createVal !i !val !byte
    | (i <= 7) = if testBit byte i
        then createVal (i + 1) (val `xor` (valueArray `V.unsafeIndex` i)) byte
        else createVal (i + 1) val byte
    | otherwise = val
  where
    valueArray = V.fromList
        ([0x1021, 0x2042, 0x4084, 0x8108, 0x1231, 0x2462, 0x48c4, 0x9188] :: [ Word16
          ]
        )

