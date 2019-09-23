module Data.TM.CalibrationTypes
  ( Calibrate(..)
  , CalibInterpolation(..)
  )
where

import           Data.TM.Value

data CalibInterpolation =
    CalibExtrapolate
    | CalibFail
    deriving (Show)

class Calibrate a where
    calibrate :: a -> TMValue -> TMValue

