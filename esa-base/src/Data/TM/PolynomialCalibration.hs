{-|
Module      : Data.TM.PolynomialCalibration
Description : data type for the polynomial calibration
Copyright   : (c) Michael Oswald, 2019
License     : BSD-3
Maintainer  : michael.oswald@onikudaki.net
Stability   : experimental
Portability : POSIX

This module provides some types used in calibrations
-}
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


import           RIO                     hiding ( (.~) )

import           Control.Lens                   ( makeLenses
                                                , (.~)
                                                )
import           Codec.Serialise
import           Data.Aeson

import           Data.Text.Short                ( ShortText )

import           General.Types                  ( ToDouble(..) )

import           Data.TM.CalibrationTypes
import           Data.TM.Value
import           Data.TM.Validity        hiding ( isValid )


-- | The polynomial calibration. Provides 5 coefficients and
-- calibrates an incoming value with the formula:
-- y = a0 + a1 * x + a2 * x^2 + a3 * x^3 + a4 * x^4
data PolynomialCalibration = PolynomialCalibration {
    _calibPName :: !ShortText
    , _calibPDescr :: !ShortText
    , _pa0 :: !Double
    , _pa1 :: !Double
    , _pa2 :: !Double
    , _pa3 :: !Double
    , _pa4 :: !Double
    }
    deriving (Eq, Show, Generic)
makeLenses ''PolynomialCalibration

instance NFData PolynomialCalibration
instance Serialise PolynomialCalibration
instance FromJSON PolynomialCalibration
instance ToJSON PolynomialCalibration where
  toEncoding = genericToEncoding defaultOptions



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
