{-|
Module      : Data.TM.Calibration
Description : Data types for calibrations
Copyright   : (c) Michael Oswald, 2019
License     : BSD-3
Maintainer  : michael.oswald@onikudaki.net
Stability   : experimental
Portability : POSIX

This module handles calibrations of TM parameters
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
    , MultiWayIf
    , TemplateHaskell
#-}
module Data.TM.Calibration
    ( Calibration(..)
    , CritCalib(..)
    , CalibContainer(..)
    , ccRLChk
    , ccValPar
    , ccCalibration

    )
where

import           RIO

import           Control.Lens                   ( makeLenses
                                                , makePrisms
                                                )
import           Codec.Serialise
import           Data.Aeson
import           Data.Text.Short

import           Data.TM.CalibrationTypes
import           Data.TM.NumericalCalibration
import           Data.TM.TextualCalibration
import           Data.TM.PolynomialCalibration
import           Data.TM.LogarithmicCalibration


-- | A sum type over all calibration types. See the individual
-- modules for details about the calibrations themselves
data Calibration =
    CalibNum NumericalCalibration
    | CalibText TextualCalibration
    | CalibPoly PolynomialCalibration
    | CalibLog LogarithmicCalibration
    deriving (Show, Generic)
makePrisms ''Calibration

instance NFData Calibration
instance Serialise Calibration
instance FromJSON Calibration
instance ToJSON Calibration where
    toEncoding = genericToEncoding defaultOptions


instance Calibrate Calibration where
    calibrate (CalibNum  x) = calibrate x
    calibrate (CalibText x) = calibrate x
    calibrate (CalibPoly x) = calibrate x
    calibrate (CalibLog  x) = calibrate x


-- | a calibration criteria. Means, that when a TM Parameter with
-- the name from '_ccRLChk' field changes it's value to the value
-- specified in the '_ccValPar' field (only integers allowed), then
-- the calibration in the field '_ccCalibration' should be active and
-- used for calibrations of this parameters values.
data CritCalib =
    CritCalib {
        _ccRLChk :: !ShortText
        , _ccValPar :: !Int
        , _ccCalibration :: Calibration
    } deriving (Show, Generic)
makeLenses ''CritCalib

instance NFData CritCalib
instance Serialise CritCalib
instance FromJSON CritCalib
instance ToJSON CritCalib where
    toEncoding = genericToEncoding defaultOptions

-- | A parameter has several possibilities to be calibrated. This
-- data type specifies which ones.
data CalibContainer =
    -- | Specifies, this parameter is uncalibrated
    CritNoCalib
    -- | Specifies, that there is onyl one calibration to be used
    | CritDirect Calibration
    -- | Specifies, that there are several calibrations, and the right
    -- one is selected via the given 'CritCalib' calibration criteria
    | Crit (Vector CritCalib)
    deriving (Show, Generic)

instance NFData CalibContainer
instance Serialise CalibContainer
instance FromJSON CalibContainer
instance ToJSON CalibContainer where
    toEncoding = genericToEncoding defaultOptions
