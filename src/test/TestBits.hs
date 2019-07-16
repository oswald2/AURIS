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

import HaskellWorks.Prim.Bits.Slow
import Data.Word
import Formatting


main :: IO ()
main = do

    let val :: Word64
        val = 0x05

        mask :: Word64
        mask = 0x38

        result = pdep64 val mask



    T.putStrLn $ sformat ("Val: " % left 8 '0' %. hex % " Mask: " % prefixHex % " Result: " % prefixHex) val mask result
    T.putStrLn $ sformat ("Val: " % prefixBin % " Mask: " % prefixBin % " Result: " % prefixBin) val mask result

    return ()