module Main (main) where

import           Test.Hspec
import           Spec.Parser (parserSpec)
import           Spec.Calibration (calibrationSpec)


main :: IO ()
main = hspec $ parallel $ do
  parserSpec
  calibrationSpec
