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
import Data.Int
import Data.Array.Unboxed
import Data.Bits

cltuArray :: UArray (Word8, Word8) Word8
cltuArray = array ((0, 0), (127, 255)) assocList


{-# INLINABLE cltuTable #-}
-- | Gets a value out of a CLTU table. This is a pre-calculated table for the parity 
-- calculations. It is only used internally in "Data.PUS.CLTU"
cltuTable :: Word8 -> Word8 -> Word8
cltuTable sreg xval = cltuArray ! (sreg, xval)


{-# INLINABLE codProcChar #-}
codProcChar :: Word8 -> Word8 -> Word8
codProcChar xvalw sregw =
    loop 0x80 (fromIntegral xvalw) (fromIntegral sregw)
    where 
        loop :: Int32 -> Int32 -> Int32 -> Word8
        loop !mask !xval !sreg 
            | mask == 0 = fromIntegral (sreg .&. 0x7F)
            | otherwise = 
                let !sreg1 = sreg `shiftL` 1
                    bit1 :: Int32
                    !bit1 = if (sreg1 .&. 0x80) /= 0 then 1 else 0
                    !bit2 = if (xval .&. mask) /= 0 then bit1 `xor` 1 else bit1
                    !sreg2 = if bit2 /= 0 then sreg1 `xor` 0x45 else sreg1
                in
                loop (mask `shiftR` 1) xval sreg2
        



assocList :: [((Word8, Word8), Word8)]
assocList =
    [ ((sreg, value), codProcChar value sreg)
        | sreg  <- [0 .. 127]
        , value <- [0 .. 255]
    ]
            