module Data.TM.Calibration
  ( Calibration(..)
  , CalibPoint(..)
  , TextCalibPoint(..)
  )
where

import           RIO

--import           Data.TM.Parameter



data CalibPoint = CalibPoint {
    _x :: !Double
    , y :: !Double
}


data TextCalibPoint = TextCalibPoint {
    _txpLower :: !Int64
    , _txpUpper :: !Int64
    , _txpText :: !Text
    }


data Calibration =
    CalibrationNumerical (Vector CalibPoint)
    | CalibrationPolynomial {
        _a0 :: !Double
        , _a1 :: !Double
        , _a2 :: !Double
        , _a3 :: !Double
        , _a4 :: !Double
    }
    | CalibrationLogarithmic {
        _l0 :: !Double
        , _l1 :: !Double
        , _l2 :: !Double
        , _l3 :: !Double
        , _l4 :: !Double
    }
    | CalibrationTextual (Vector TextCalibPoint)
