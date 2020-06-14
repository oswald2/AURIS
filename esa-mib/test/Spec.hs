module Main (main) where

import           Test.Hspec
import           Spec.Parser (parserSpec)


main :: IO ()
main = hspec $ parallel $ do
  parserSpec
