{-# LANGUAGE OverloadedStrings
    , BangPatterns
    , GeneralizedNewtypeDeriving
    , DeriveGeneric
    , RecordWildCards
    , NoImplicitPrelude
    , BinaryLiterals
    , NumericUnderscores
    , FlexibleInstances
    , GADTs
    , ExistentialQuantification
#-}
module Main
where

import RIO
import qualified Data.Text.IO as T
import qualified RIO.ByteString as B
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as V

import Data.Bits
import Data.Bits.Pdep
import Data.Word
import Formatting

import Control.Monad.ST

import General.Hexdump
import General.SetBitField
import General.Types

import Data.Vector.Storable.ByteString
import HaskellWorks.Data.Vector.AsVector64
import HaskellWorks.Data.Vector.AsVector8
import           Criterion.Main


main :: IO ()
main = do
    defaultMain
        [ bgroup
            "encoding"
            [ bench "Encode10" $ whnfIO $ encodeValues 10
            , bench "Encode10R" $ whnfIO $ encodeValuesR 10
            , bench "Encode4096" $ whnfIO $ encodeValues 4096
            , bench "Encode4096R" $ whnfIO $ encodeValuesR 4096
            ]
        ]


--encodeValues :: Int -> IO ()
encodeValues size = do
    let res = runST $ do
            vec <- V.new size

            mapM (setValue vec) [(mkBitOffset b, 3, 5) | b <- take (size * 8 `div` 3) [0,3 .. ]]

            V.freeze vec

    return res
    where
        setValue vec (bitOffset, width, value) = setBitField vec bitOffset width value

encodeValuesR size = do
    let res = runST $ do
            vec <- V.new size

            mapM (setValue vec) [(mkBitOffset b, 3, 5) | b <- take (size * 8 `div` 3) [0,3 .. ]]

            V.freeze vec

    return res
    where
        setValue vec (bitOffset, width, value) = setBitFieldR vec bitOffset width value



-- main :: IO ()
-- main = do

--     let val :: Word64
--         val = 0x05

--         mask :: Word64
--         mask = 0x380

--         result = pdep val mask

--         vecLen = 128

--         --bytes = B.pack $ take vecLen [ fromIntegral (x `rem` 256) | x <- [0..] :: [Int] ]
--         bytes = B.replicate vecLen 0

--         vec = asVector64 bytes


--     T.putStrLn $ sformat ("Val: " % left 16 '0' %. hex % " Mask: " % prefixHex % " Result: " % prefixHex) val mask result
--     T.putStrLn $ sformat ("Val: " % prefixBin % " Mask: " % prefixBin % " Result: " % prefixBin) val mask result


--     let firstVal = byteSwap64 (vec V.! 0)
--         firstValMod = firstVal .|. pdep val (byteSwap64 mask)

--     vec1 <- V.thaw vec

--     V.write vec1 0 (byteSwap64 firstValMod)

--     vec2 <- V.freeze vec1

--     T.putStrLn $ sformat ("First val: " % left 16 '0' %. hex) firstVal
--     T.putStrLn $ sformat ("First val mod: " % left 16 '0' %. hex) firstValMod

--     let vec3 = asVector8 vec2
--         bytestring2 = vectorToByteString vec3

--     T.putStrLn $ hexdumpBS bytestring2


--     let setB = runST $ do
--             vec <- V.new vecLen
--             setBitField vec 7 3 5
--             vec2 <- V.freeze vec
--             pure (vectorToByteString vec2)

--     T.putStrLn $ "\nsetBitField:\n\n" <> hexdumpBS setB


--     return ()