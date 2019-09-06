{-|
Module      : General.SetBitField
Description : Functions for setting values. Used in encoding the parameters into binary form
Copyright   : (c) Michael Oswald, 2019
License     : BSD-3
Maintainer  : michael.oswald@onikudaki.net
Stability   : experimental
Portability : POSIX

This module provides functions for encoding values down to binary form. For efficancy this 
functions operator on a mutable vector in the 'ST' Monad.
-}
{-# LANGUAGE OverloadedStrings
    , BangPatterns
    , NoImplicitPrelude
#-}
module General.SetBitField
  ( SetValue(..)
  , setBitField
  , setBitFieldR
  , copyBS
  )
where

import           RIO
import qualified RIO.ByteString                as B

import           Control.Monad.ST

import qualified Data.Vector.Storable.Mutable  as VS
import           Data.Bits
import           Data.Binary.IEEE754

import           General.Types

--import           HaskellWorks.Prim.Bits


-- | This class is a generic class to set a value in a vector
-- aligned to byte boundaries with the given Endianess
class SetValue a where
    setValue :: VS.MVector s Word8 -> ByteOffset -> Endian -> a -> ST s ()


instance SetValue Int8 where
  setValue vec off _ val =
    VS.unsafeWrite vec (unByteOffset off) (fromIntegral val)
  {-# INLINABLE setValue #-}

instance SetValue Word8 where
  setValue vec off _ = VS.unsafeWrite vec (unByteOffset off)
  {-# INLINABLE setValue #-}

instance SetValue Word16 where
  setValue vec off' BiE val = do
    let !val1 = fromIntegral $ val .&. 0xFF00 `shiftR` 8
        !val2 = fromIntegral $ val .&. 0xFF
        off   = unByteOffset off'
    VS.unsafeWrite vec off val1
    VS.unsafeWrite vec (off + 1) val2
  setValue vec off' LiE val = do
    let !val2 = fromIntegral $ val .&. 0xFF00 `shiftR` 8
        !val1 = fromIntegral $ val .&. 0xFF
        off   = unByteOffset off'
    VS.unsafeWrite vec off val1
    VS.unsafeWrite vec (off + 1) val2
  {-# INLINABLE setValue #-}

instance SetValue Int16 where
  setValue vec off endian val =
    setValue vec off endian (fromIntegral val :: Word16)
  {-# INLINABLE setValue #-}

instance SetValue Word32 where
  setValue vec off' BiE val = do
    let !val1 = fromIntegral $ val .&. 0xFF000000 `shiftR` 24
        !val2 = fromIntegral $ val .&. 0x00FF0000 `shiftR` 16
        !val3 = fromIntegral $ val .&. 0x0000FF00 `shiftR` 8
        !val4 = fromIntegral $ val .&. 0x000000FF
        off   = unByteOffset off'
    VS.unsafeWrite vec off val1
    VS.unsafeWrite vec (off + 1) val2
    VS.unsafeWrite vec (off + 2) val3
    VS.unsafeWrite vec (off + 3) val4
  setValue vec off' LiE val = do
    let !val4 = fromIntegral $ val .&. 0xFF000000 `shiftR` 24
        !val3 = fromIntegral $ val .&. 0x00FF0000 `shiftR` 16
        !val2 = fromIntegral $ val .&. 0x0000FF00 `shiftR` 8
        !val1 = fromIntegral $ val .&. 0x000000FF
        off   = unByteOffset off'
    VS.unsafeWrite vec off val1
    VS.unsafeWrite vec (off + 1) val2
    VS.unsafeWrite vec (off + 2) val3
    VS.unsafeWrite vec (off + 3) val4
  {-# INLINABLE setValue #-}

instance SetValue Int32 where
  setValue vec off endian val =
    setValue vec off endian (fromIntegral val :: Word32)
  {-# INLINABLE setValue #-}


instance SetValue Word64 where
  setValue vec off' BiE val = do
    let !val1 = fromIntegral $ val .&. 0xFF00000000000000 `shiftR` 56
        !val2 = fromIntegral $ val .&. 0x00FF000000000000 `shiftR` 48
        !val3 = fromIntegral $ val .&. 0x0000FF0000000000 `shiftR` 40
        !val4 = fromIntegral $ val .&. 0x000000FF00000000 `shiftR` 32
        !val5 = fromIntegral $ val .&. 0x00000000FF000000 `shiftR` 24
        !val6 = fromIntegral $ val .&. 0x0000000000FF0000 `shiftR` 16
        !val7 = fromIntegral $ val .&. 0x000000000000FF00 `shiftR` 8
        !val8 = fromIntegral $ val .&. 0x00000000000000FF
        off   = unByteOffset off'
    VS.unsafeWrite vec off val1
    VS.unsafeWrite vec (off + 1) val2
    VS.unsafeWrite vec (off + 2) val3
    VS.unsafeWrite vec (off + 3) val4
    VS.unsafeWrite vec (off + 4) val5
    VS.unsafeWrite vec (off + 5) val6
    VS.unsafeWrite vec (off + 6) val7
    VS.unsafeWrite vec (off + 7) val8
  setValue vec off' LiE val = do
    let !val8 = fromIntegral $ val .&. 0xFF00000000000000 `shiftR` 56
        !val7 = fromIntegral $ val .&. 0x00FF000000000000 `shiftR` 48
        !val6 = fromIntegral $ val .&. 0x0000FF0000000000 `shiftR` 40
        !val5 = fromIntegral $ val .&. 0x000000FF00000000 `shiftR` 32
        !val4 = fromIntegral $ val .&. 0x00000000FF000000 `shiftR` 24
        !val3 = fromIntegral $ val .&. 0x0000000000FF0000 `shiftR` 16
        !val2 = fromIntegral $ val .&. 0x000000000000FF00 `shiftR` 8
        !val1 = fromIntegral $ val .&. 0x00000000000000FF
        off   = unByteOffset off'
    VS.unsafeWrite vec off val1
    VS.unsafeWrite vec (off + 1) val2
    VS.unsafeWrite vec (off + 2) val3
    VS.unsafeWrite vec (off + 3) val4
    VS.unsafeWrite vec (off + 4) val5
    VS.unsafeWrite vec (off + 5) val6
    VS.unsafeWrite vec (off + 6) val7
    VS.unsafeWrite vec (off + 7) val8
  {-# INLINABLE setValue #-}

instance SetValue Int64 where
  setValue vec off endian val =
    setValue vec off endian (fromIntegral val :: Word64)
  {-# INLINABLE setValue #-}

instance SetValue Double where
  setValue vec off endian val = setValue vec off endian (doubleToWord val)
  {-# INLINABLE setValue #-}

instance SetValue Float where
  setValue vec off endian val = setValue vec off endian (floatToWord val)
  {-# INLINABLE setValue #-}


-- | Copy the value of a ByteString to the given vector on the
-- given byte-offset. It is assumed, that the vector is large
-- enough
{-# INLINABLE copyBS #-}
copyBS :: VS.MVector s Word8 -> ByteOffset -> ByteString -> ST s ()
copyBS vec off' bs = go 0 (B.length bs)
 where
  off = unByteOffset off'
  go !idx !blength
    | idx >= blength = pure ()
    | otherwise = do
      VS.unsafeWrite vec (off + idx) (bs `B.index` idx)
      go (idx + 1) blength



{-# INLINABLE byteIndex' #-}
byteIndex' :: BitOffset -> BitSize -> Int
byteIndex' bitOffset bitWidth =
  unByteOffset
    .              toByteOffset
    $              bitOffset
    `addBitOffset` bitWidth
    `subBitOffset` 1


-- | Sets a given Word64 value (my be smaller than Word64) as
-- a binary value into the vector. This function may resize the
-- vector if the offset is outside it's length. Returns either
-- the initial vector or the new, resized vector when resizing
-- happened
-- @bytes@ is the initial vector
-- @bitOffset@ specifies the offset in bits into the vector where
-- the value should be set
-- @bitWidth@ specifies the width of the value in bits (smaller than 64)
-- @value@ gives the value as 'Word64' to be set.
{-# INLINABLE setBitFieldR #-}
setBitFieldR
  :: VS.MVector s Word8
  -> BitOffset
  -> BitSize
  -> Word64
  -> ST s (VS.MVector s Word8)
setBitFieldR bytes' bitOffset bitWidth value = do
  let !byteIndex = byteIndex' bitOffset bitWidth
      bmask :: Word64
      !bmask =
        let x = if bitWidth < 64 then 1 `shiftL` unBitSize bitWidth else 0
        in  x - 1
      !r = unBitOffset (bitOffset `addBitOffset` bitWidth) .&. 0x07
      !s = 8 - r
      getBytes !b'
        | byteIndex < VS.length b' = return b'
        | otherwise = do
          let newSize = byteIndex - VS.length b' + 1
          v <- VS.grow bytes' newSize
          fill v (VS.length b') byteIndex 0
          return v
      setb !b !idx !val !mask'
        | mask' < 255 = return (idx, val, mask')
        | otherwise = do
          VS.write b idx (fromIntegral val)
          setb b (idx - 1) (val `shiftR` 8) (mask' `shiftR` 8)

  -- grow the vector if necessary
  b               <- getBytes bytes'

  (!bi, !va, !ma) <- if r /= 0
    then do
      v <- VS.read b byteIndex
      let !v1 = complement (bmask `shiftL` s)
          !v2 = fromIntegral v .&. v1
          !v3 = value `shiftL` s
          !v4 = fromIntegral (v2 .|. v3)
      VS.write b byteIndex v4
      return (byteIndex - 1, value `shiftR` r, bmask `shiftR` r)
    else return (byteIndex, value, bmask)
  (!bi2, !va2, !ma2) <- setb b bi va ma

  when (ma2 > 0) $ do
    v <- fromIntegral <$> VS.read b bi2
    let !v3 = fromIntegral (v1 .|. v2)
        !v2 = va2 .&. ma2
        !v1 = v .&. complement ma2
    VS.write b bi2 v3
  return b
 where
  fill !v !st !end !val = go st
   where
    go i
      | i > end = return ()
      | otherwise = do
        VS.write v i val
        go (i + 1)




-- | Sets a given Word64 value (my be smaller than Word64) as
-- a binary value into the vector. This function may assumes that
-- the vector is large enough and performs now bounds check.
-- @bytes@ is the initial vector
-- @bitOffset@ specifies the offset in bits into the vector where
-- the value should be set
-- @bitWidth@ specifies the width of the value in bits (smaller than 64)
-- @value@ gives the value as 'Word64' to be set.
{-# INLINABLE setBitField #-}
setBitField :: VS.MVector s Word8 -> BitOffset -> BitSize -> Word64 -> ST s ()
setBitField bytes bitOffset bitWidth value = do
  let !byteIndex = byteIndex' bitOffset bitWidth
      bmask :: Word64
      !bmask =
        let x = if bitWidth < 64 then 1 `shiftL` unBitSize bitWidth else 0
        in  x - 1
      !r = unBitOffset (bitOffset `addBitOffset` bitWidth) .&. 0x07
      !s = 8 - r
      setb !b !idx !val !mask'
        | mask' < 255 = return (idx, val, mask')
        | otherwise = do
          VS.write b idx (fromIntegral val)
          setb b (idx - 1) (val `shiftR` 8) (mask' `shiftR` 8)

  (!bi, !va, !ma) <- if r /= 0
    then do
      v <- VS.read bytes byteIndex
      let !v1 = complement (bmask `shiftL` s)
          !v2 = fromIntegral v .&. v1
          !v3 = value `shiftL` s
          !v4 = fromIntegral (v2 .|. v3)
      VS.write bytes byteIndex v4
      return (byteIndex - 1, value `shiftR` r, bmask `shiftR` r)
    else return (byteIndex, value, bmask)
  (!bi2, !va2, !ma2) <- setb bytes bi va ma

  when (ma2 > 0) $ do
    v <- fromIntegral <$> VS.read bytes bi2
    let !v3 = fromIntegral (v1 .|. v2)
        !v2 = va2 .&. ma2
        !v1 = v .&. complement ma2
    VS.write bytes bi2 v3




