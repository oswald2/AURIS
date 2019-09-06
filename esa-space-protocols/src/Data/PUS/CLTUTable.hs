{-|
Module      : Data.PUS.CLTUTable
Description : Provides the lookup table for parity calculations
Copyright   : (c) Michael Oswald, 2019
License     : BSD-3
Maintainer  : michael.oswald@onikudaki.net
Stability   : experimental
Portability : POSIX

Provides the lookup table for parity calculations
-}
{-# LANGUAGE BangPatterns 
#-}
module Data.PUS.CLTUTable
    (
        cltuTable
        , codProcChar
    )
where


import Data.Word 
import Data.Vector.Unboxed as V
import Data.Bits


{-# INLINABLE cltuVector #-}
cltuVector :: V.Vector Word8
cltuVector = 
    let len = 128 * 256
        gen :: Int -> Word8
        gen x = 
            let !xval = x `rem` 256
                !sreg = (x - xval) `quot` 256
                !byte = codProcChar xval sreg
            in byte
        !v = V.generate len gen
    in
    v


{-# INLINABLE cltuTable #-}
-- | Gets a value out of a CLTU table. This is a pre-calculated table for the parity 
-- calculations. It is only used internally in "Data.PUS.CLTU"
cltuTable :: Word8 -> Word8 -> Word8
cltuTable !sreg !xval = 
    let !res = cltuVector V.! (fromIntegral sreg * 256 + fromIntegral xval)
    in 
    res


{-# INLINABLE codProcChar #-}
-- | Helper function for table calculation. It is only exported to be available 
-- in tests and benchmarks
codProcChar :: Int -> Int -> Word8
codProcChar !xvalw !sregw =
    loop 0x80 xvalw sregw
    where 
        loop :: Int -> Int -> Int -> Word8
        loop !mask !xval !sreg 
            | mask == 0 = fromIntegral (sreg .&. 0x7F)
            | otherwise = 
                let !sreg1 = sreg `shiftL` 1
                    bit1 :: Int
                    !bit1 = if (sreg1 .&. 0x80) /= 0 then 1 else 0
                    !bit2 = if (xval .&. mask) /= 0 then bit1 `xor` 1 else bit1
                    !sreg2 = if bit2 /= 0 then sreg1 `xor` 0x45 else sreg1
                in
                loop (mask `shiftR` 1) xval sreg2
        


