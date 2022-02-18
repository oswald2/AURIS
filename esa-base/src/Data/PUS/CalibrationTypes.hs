{-|
Module      : Data.PUS.CalibrationTypes
Description : General data types for calibrations
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
#-}
module Data.PUS.CalibrationTypes
    ( Calibrate(..)
    , TcCalibration(..)
    , CalibInterpolation(..)
    , toCalibInterpolation
    , calibInterpolationBuilder
    ) where

import           Codec.Serialise
import           Data.Aeson
import           RIO

import           Data.TM.Value

import           Text.Builder                  as TB



-- | Specifies what a calibration should do when the
-- given value falls out of the range of the calibration
-- (only applicable to 'NumericCalibration').
data CalibInterpolation =
    -- | The calibration will extrapolate the value from the last 2 points within the range
    CalibExtrapolate
    -- | The calibration will indicate an error
    | CalibFail
    deriving (Eq, Ord, Enum, Show, Read, Generic)

instance NFData CalibInterpolation
instance Serialise CalibInterpolation
instance FromJSON CalibInterpolation
instance ToJSON CalibInterpolation where
    toEncoding = genericToEncoding defaultOptions

calibInterpolationBuilder :: CalibInterpolation -> TB.Builder
calibInterpolationBuilder CalibExtrapolate = "Extrapolate"
calibInterpolationBuilder CalibFail        = "Fail"


-- | Converts from a Charactor to the interpolation type.
-- Specified in the SCOS-2000 MIB ICD 6.9
toCalibInterpolation :: Char -> CalibInterpolation
toCalibInterpolation 'P' = CalibExtrapolate
toCalibInterpolation _   = CalibFail


-- | This class provides the function to calibrate a value.
-- The first value is the calibration used.
class Calibrate a where
    calibrate :: a -> TMValue -> TMValue


-- | This class provides the functionality for calibration and 
-- de-calibration of TC values. This is not simply based on 
-- TMValues, but can span more types
--
-- Type 'a' is the calibration, 'b' is the from type and 'c' is the 
-- to type. Calibrations muste be possible in both directions
class TcCalibration a b c where
    tcCalibrate :: a -> b -> Maybe c
    tcDeCalibrate :: a -> c -> Maybe b


