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
    , TemplateHaskell
#-}
module Data.TM.PolynomialCalibration
  ( PolynomialCalibration(..)
  , calibPName
  , calibPDescr
  , pa0
  , pa1
  , pa2
  , pa3
  , pa4
  )
where


import           RIO

import           Control.Lens                   ( makeLenses
                                                , (.~)
                                                )

import           Data.Text.Short                ( ShortText )

import           General.Types                  ( ToDouble(..) )

import           Data.TM.CalibrationTypes
import           Data.TM.Value
import           Data.TM.Validity        hiding ( isValid )


data PolynomialCalibration = PolynomialCalibration {
    _calibPName :: !ShortText
    , _calibPDescr :: !ShortText
    , _pa0 :: !Double
    , _pa1 :: !Double
    , _pa2 :: !Double
    , _pa3 :: !Double
    , _pa4 :: !Double
    }
    deriving (Show)
makeLenses ''PolynomialCalibration


instance Calibrate PolynomialCalibration where
  calibrate calib rawValue | isValid rawValue = if isNumeric rawValue
    then
      let x = toDouble rawValue
      in  rawValue & tmvalValue .~ TMValDouble (interpolate calib x)
    else setValidity rawValue validitySetWrongType
  calibrate _calib rawValue = rawValue



interpolate :: PolynomialCalibration -> Double -> Double
interpolate PolynomialCalibration {..} x =
  let !xx     = x * x
      !xxx    = xx * x
      !xxxx   = xxx * x
      !result = _pa0 + _pa1 * x + _pa2 * xx + _pa3 * xxx + _pa4 * xxxx
  in  result
