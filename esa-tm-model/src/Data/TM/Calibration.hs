module Data.TM.Calibration
  ( CalibrationTextual
  , CalibrationNumerical
  , CalibrationPolynomial
  , CalibrationLogarithmic
  )
where

import           Data.TM.Parameter


data CalibrationTextual = CalibrationTextual

data CalibrationNumerical = CalibrationNumerical

data CalibrationPolynomial = CalibrationPolynomial

data CalibrationLogarithmic = CalibrationLogarithmic


class CalibrationEval a where
    calibrate :: a -> TMParameter a b -> TMParameter a b
    deCalibrate :: b -> TMParameter a b -> TMParameter a b
