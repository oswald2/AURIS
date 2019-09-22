module Data.TM.Calibration
  ( Calibration(..)
  , CalibPoint(..)
  , TextCalibPoint(..)
  )
where

import           RIO

--import           Data.TM.Parameter
import           Data.Text.Short                ( ShortText )


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
        _calPName :: !ShortText
        , _calPDescription :: !ShortText
        , _a0 :: !Double
        , _a1 :: !Double
        , _a2 :: !Double
        , _a3 :: !Double
        , _a4 :: !Double
    }
    | CalibrationLogarithmic {
        _calLName :: !ShortText
        , _calLDescription :: !ShortText
        , _l0 :: !Double
        , _l1 :: !Double
        , _l2 :: !Double
        , _l3 :: !Double
        , _l4 :: !Double
    }
    | CalibrationTextual (Vector TextCalibPoint)
