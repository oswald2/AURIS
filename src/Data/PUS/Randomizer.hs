{-|
Module      : Data.PUS.Randomizer
Description : Randomizing functionality for CLTUs
Copyright   : (c) Michael Oswald, 2019
License     : BSD-3
Maintainer  : michael.oswald@onikudaki.net
Stability   : experimental
Portability : POSIX

This module provides the randomization functionality according to the ESA PUS standard. 
This is a xor with a pre-defined array of values (which can be shifted) to distribute the 
1's and 0's of the data in the transmission to the satellite more even.

Note that not all missions use this.
-}
{-# LANGUAGE BangPatterns 
#-}
module Data.PUS.Randomizer
    (
        Randomizer
        , defaultStartValue
        , initialize
        , randomize
    )
where


import Data.Vector.Unboxed
import Data.Word
import Data.Bits
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B



-- | The type for the Randomizer. A Randomizer does not exactyl random things. 
-- It provides a sequence of bytes which, when xor'ed with the data byes of 
-- a CLTU, provides a more evenly distribution of 1's and 0'es for 
-- transmission to the satellite
newtype Randomizer = Randomizer (Vector Word8)


-- | The default start value for the randomizing process
defaultStartValue :: Word8
defaultStartValue = 0xFF


-- | generates an initial randomizer from a start value
initialize :: Word8 -> Randomizer
initialize start = Randomizer $ generate 8 f
    where
        f i = 
            let d = (1 :: Word8) `shiftL` i
            in 
            (d .&. start) `shiftR` i

-- | gets the next byte from a randomizer. If peek is True, also 
-- a new randomizer will be generated, otherwise the passed 
-- randomizer will be returned.
getNextByte :: Randomizer -> Bool -> (Randomizer, Word8)
getNextByte (Randomizer rand) peek = 
    loop rand 0 7
    where
        loop :: Vector Word8 -> Word32 -> Int -> (Randomizer, Word8)
        loop v d 0 = (Randomizer v, fromIntegral d)
        loop v d i = 
            let (newV, newD) = proc v d 
            in 
                loop newV newD (i - 1)

        proc :: Vector Word8 -> Word32 -> (Vector Word8, Word32)
        proc v d =
            let !topBit = v ! 0 
                    `xor` v ! 1
                    `xor` v ! 2
                    `xor` v ! 3
                    `xor` v ! 4
                    `xor` v ! 6
                d1 = d `shiftL` 1 
                !d2 = if v ! 0 /= 0 then d1 + 1 else d1

                newV = if peek
                    then
                        fromList [
                            v ! 1
                            , v ! 2
                            , v ! 3
                            , v ! 4
                            , v ! 5
                            , v ! 6
                            , v ! 7
                            , topBit
                        ]
                    else v
            in
            (newV, d2)


-- | This function performs the actual randomization of data bytes. @r is the start-randomizer,
-- @peek indicates if the randomizer is also updated with values. The given ByteString is then
-- converted and a new Randomizer as well as the converted ByteString are returned
randomize :: Randomizer -> Bool -> ByteString -> (Randomizer, ByteString)
randomize r peek = B.mapAccumL f r
    where f r1 x = let (r2, next) = getNextByte r1 peek in (r2, (x `xor` next))
            
            