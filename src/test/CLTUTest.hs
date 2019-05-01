{-# LANGUAGE TemplateHaskell #-}
module CLTUTest
where


import Data.PUS.CLTU
import Data.PUS.Config

import qualified Data.ByteString.Lazy as B

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Gen.QuickCheck as Gen
import qualified Hedgehog.Range as Range

genCLTU :: Gen CLTU
genCLTU =
    let len = Range.linear 8 65535
        lst = cltuNew . B.fromStrict <$> Gen.bytes len
    in
    lst

    
prop_loop :: Property
prop_loop = 
    property $ do 
        x <- forAll genCLTU
        let e = encode defaultConfig x
            d = decode defaultConfig e 
        case d of 
            Left err -> failure
            Right x1 -> x === x1


tests :: IO Bool
tests = checkParallel $$(discover)