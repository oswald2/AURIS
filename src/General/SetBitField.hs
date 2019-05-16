{-# LANGUAGE OverloadedStrings
    , BangPatterns
    , NoImplicitPrelude 
#-}
module General.SetBitField
    (setBitField)
where

import RIO

import Control.Monad.ST

import qualified Data.Vector.Storable.Mutable as VS 
import Data.Bits




setBitField
    :: VS.MVector s Word8 -> Int -> Int -> Word64 -> ST s (VS.MVector s Word8)
setBitField bytes' bitOffset bitWidth value = do
    let !byteIndex = (bitOffset + bitWidth - 1) `div` 8
        bmask :: Word64
        !bmask =
            let x = if bitWidth < 64 then 1 `shiftL` bitWidth else 0 in x - 1
        !r = (bitOffset + bitWidth) `mod` 8
        !s = 8 - r
        getBytes !b'
            | byteIndex < VS.length b' = return b'
            | otherwise = do
                let newSize = (byteIndex - VS.length b' + 1)
                v <- VS.grow bytes' newSize
                fill v (VS.length b') byteIndex 0
                return v
        setb !b !idx !val !mask'
            | mask' < 255 = return (idx, val, mask')
            | otherwise = do
                VS.write b idx (fromIntegral val)
                setb b (idx - 1) (val `shiftR` 8) (mask' `shiftR` 8)

    -- grow the vector if necessary
    b            <- getBytes bytes'

    (bi, va, ma) <- if (r /= 0)
        then do
            v <- VS.read b byteIndex
            let v1 = complement (bmask `shiftL` s)
                v2 = (fromIntegral v .&. v1)
                v3 = (value `shiftL` s)
                v4 = fromIntegral (v2 .|. v3)
            VS.write b byteIndex v4
            return $ (byteIndex - 1, (value `shiftR` r), (bmask `shiftR` r))
        else return (byteIndex, value, bmask)
    (bi2, va2, ma2) <- setb b bi va ma

    when (ma2 > 0) $ do
        v <- fromIntegral <$> VS.read b bi2
        let v3 = fromIntegral (v1 .|. v2)
            v2 = (va2 .&. ma2)
            v1 = (v .&. complement ma2)
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