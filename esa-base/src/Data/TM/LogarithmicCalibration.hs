{-|
Module      : Data.TM.LogarithmicCalibration
Description : data type for the logarithmic calibration
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
module Data.TM.LogarithmicCalibration
    ( LogarithmicCalibration(..)
    , calibLName
    , calibLDescr
    , la0
    , la1
    , la2
    , la3
    , la4
    , logarithmicCalibrationBuilder
    ) where

import           RIO                     hiding ( (.~) )
--import qualified RIO.Text                      as T

import           Control.Lens                   ( (.~)
                                                , makeLenses
                                                )

import           Data.Text.Short                ( ShortText )
import qualified Data.Text.Short               as ST

import           Codec.Serialise
import           Data.Aeson

import           Data.PUS.CalibrationTypes
import           Data.TM.Validity        hiding ( isValid )
import           Data.TM.Value

import           Text.Builder                  as TB

import           General.Types


-- | The logarithmic calibration. Provides 5 coefficients and
-- calibrates an incoming value with the formula:
-- y = 1/(a0 + a1 * ln(x) + a2 * ln^2(x) + a3 * ln^3(x) + a4 * ln^4(x))
data LogarithmicCalibration = LogarithmicCalibration
    { _calibLName  :: !ShortText
    , _calibLDescr :: !ShortText
    , _la0         :: !Double
    , _la1         :: !Double
    , _la2         :: !Double
    , _la3         :: !Double
    , _la4         :: !Double
    }
    deriving (Eq, Show, Generic)
makeLenses ''LogarithmicCalibration

instance NFData LogarithmicCalibration
instance Serialise LogarithmicCalibration
instance FromJSON LogarithmicCalibration
instance ToJSON LogarithmicCalibration where
    toEncoding = genericToEncoding defaultOptions


logarithmicCalibrationBuilder :: LogarithmicCalibration -> Word16 -> TB.Builder
logarithmicCalibrationBuilder calib indent =
    indentBuilder indent
        <> text "<b>Type:</b>           Logarithmic"
        <> newLineIndentBuilder indent (padRight 23 (text "<b>Name:</b> "))
        <> text (ST.toText (_calibLName calib))
        <> newLineIndentBuilder indent (padRight 23 (text "<b>Description:</b> "))
        <> text (ST.toText (_calibLDescr calib))
        <> newLineIndentBuilder indent (text "A0=")
        <> fixedDouble 16 (_la0 calib)
        <> text " A1="
        <> fixedDouble 16 (_la1 calib)
        <> text " A2="
        <> fixedDouble 16 (_la2 calib)
        <> text " A3="
        <> fixedDouble 16 (_la3 calib)
        <> text " A4="
        <> fixedDouble 16 (_la4 calib)



instance Calibrate LogarithmicCalibration where
    calibrate calib rawValue | isValid rawValue = if isNumeric rawValue
        then
            let x = toDouble rawValue
            in  rawValue & tmvalValue .~ TMValDouble (interpolate calib x)
        else setValidity rawValue validitySetWrongType
    calibrate _calib rawValue = rawValue

interpolate :: LogarithmicCalibration -> Double -> Double
interpolate LogarithmicCalibration {..} x =
    let
        !lx  = log x
        !l2x = lx * lx
        !l3x = l2x * lx
        !l4x = l3x * lx
        !result =
            1.0 / (_la0 + _la1 * lx + _la2 * l2x + _la3 * l3x + _la4 * l4x)
    in
        result
