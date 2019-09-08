module Data.TM.Calibration
  ( CalibrationTextual
  , CalibrationNumerical
  , CalibrationPolynomial
  , CalibrationLogarithmic
  , mkCalibrationPolynomial
  , CalibrationSum(..)
  )
where

import           Data.TM.Parameter


data CalibrationTextual = CalibrationTextual

data CalibrationNumerical = CalibrationNumerical

data CalibrationPolynomial = CalibrationPolynomial {
    _a0 :: Double
    , _a1 :: Double
    , _a2 :: Double
    , _a3 :: Double
    , _a4 :: Double
    } deriving (Show)

mkCalibrationPolynomial :: Double -> Double -> Double -> Double -> Double -> CalibrationPolynomial
mkCalibrationPolynomial = CalibrationPolynomial


data CalibrationLogarithmic = CalibrationLogarithmic


data CalibrationSum = 
    CalibText CalibrationTextual
    | CalibNum CalibrationNumerical
    | CalibPoly CalibrationPolynomial
    | CalibLog CalibrationLogarithmic


class CalibrationEval a where
    calibrate :: a -> TMParameter a b -> TMParameter a b
    deCalibrate :: b -> TMParameter a b -> TMParameter a b


instance CalibrationEval CalibrationTextual where
    calibrate c p = p 
    deCalibrate c p = p

instance CalibrationEval CalibrationNumerical where
    calibrate c p = p 
    deCalibrate c p = p
    
instance CalibrationEval CalibrationPolynomial where
    calibrate c p = p
    deCalibrate c p = p

instance CalibrationEval CalibrationLogarithmic where
    calibrate c p = p 
    deCalibrate c p = p
                        

