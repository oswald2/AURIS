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
    , NoImplicitPrelude
#-}
module Data.PUS.Randomizer
    (
        Randomizer
        , defaultStartValue
        , initialize
        , randomize
    )
where

import RIO

import RIO.State

import qualified Data.Vector.Unboxed as U
import Data.Bits
import RIO.ByteString



-- | The type for the Randomizer. A Randomizer does not exactyl random things. 
-- It provides a sequence of bytes which, when xor'ed with the data byes of 
-- a CLTU, provides a more evenly distribution of 1's and 0'es for 
-- transmission to the satellite
newtype Randomizer = Randomizer (U.Vector Word8)


-- | The default start value for the randomizing process
defaultStartValue :: Word8
defaultStartValue = 0xFF


-- | generates an initial randomizer from a start value
initialize :: Word8 -> Randomizer
initialize start = Randomizer $ U.generate 8 f
    where
        f i = 
            let d = (1 :: Word8) `shiftL` i
            in 
            (d .&. start) `shiftR` i

-- | gets the next byte from a randomizer. If peek is True, also 
-- a new randomizer will be generated, otherwise the passed 
-- randomizer will be returned.
{-getNextByte' :: Randomizer -> Bool -> (Randomizer, Word8)
getNextByte' (Randomizer rand) peek = 
    loop rand 0 7
    where
        loop :: U.Vector Word8 -> Word32 -> Int -> (Randomizer, Word8)
        loop v d 0 = (Randomizer v, fromIntegral d)
        loop v d i = 
            let (newV, newD) = proc v d 
            in 
                loop newV newD (i - 1)

        proc :: U.Vector Word8 -> Word32 -> (U.Vector Word8, Word32)
        proc v d =
            let !topBit = v U.! 0 
                    `xor` v U.! 1
                    `xor` v U.! 2
                    `xor` v U.! 3
                    `xor` v U.! 4
                    `xor` v U.! 6
                !d1 = d `shiftL` 1 
                !d2 = if v U.! 0 /= 0 then d1 + 1 else d1

                newV = if peek
                    then
                        U.tail v `U.snoc` topBit
                    else v
            in
            (newV, d2)
-}

-- | gets the next byte from a randomizer. If peek is True, also 
-- a new randomizer will be generated and put into the State
getNextByte :: Bool -> State Randomizer Word8
getNextByte peek = do
    fromIntegral <$> foldM (\d _ -> proc d) 0 ([1..8] :: [Int])
    where
        proc :: Word32 -> State Randomizer Word32
        proc d = do
            Randomizer v <- get
            let !topBit = v U.! 0 
                    `xor` v U.! 1
                    `xor` v U.! 2
                    `xor` v U.! 3
                    `xor` v U.! 4
                    `xor` v U.! 6
                !d1 = d `shiftL` 1 
                !d2 = if v U.! 0 /= 0 then d1 + 1 else d1

            if peek
                then put $ Randomizer (U.tail v `U.snoc` topBit)
                else pure () 
            pure d2

{-randomize' :: Randomizer -> Bool -> ByteString -> (Randomizer, ByteString)
randomize' r peek = mapAccumL f r
    where f r1 x = let (r2, next) = getNextByte' r1 peek in (r2, (x `xor` next))
-}          

-- | This function performs the actual randomization of data bytes. 
-- @peek indicates if the randomizer is also updated with values. The given ByteString is then
-- converted and returned
randomize :: Bool -> ByteString -> State Randomizer ByteString
randomize peek block = do
    pack <$> mapM proc (unpack block)
    where
        proc byte = xor byte <$> getNextByte peek
                