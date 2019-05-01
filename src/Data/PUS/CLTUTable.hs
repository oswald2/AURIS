{-# LANGUAGE BangPatterns 
#-}
module Data.PUS.CLTUTable
    (
        cltuTable
    )
where


import Data.Word 
import Data.Int
import Data.Array.Unboxed
import Data.Bits



cltuArray :: UArray (Word8, Word8) Word8
cltuArray = array ((0, 0), (127, 255)) assocList


{-# INLINABLE cltuTable #-}
cltuTable :: Word8 -> Word8 -> Word8
cltuTable x y = cltuArray ! (x, y)


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
                    !bit1 = if (sreg .&. 0x80) /= 0 then 1 else 0
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
            