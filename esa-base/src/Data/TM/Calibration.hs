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
    , calibBuilder
    , critCalibBuilder
    , calibContainerBuilder
    ) where

import           RIO
import qualified RIO.Text                      as T
import qualified RIO.Vector                    as V

import           Codec.Serialise
import           Control.Lens                   ( makeLenses
                                                , makePrisms
                                                )
import           Data.Aeson
import           Data.Text.Short                ( ShortText )
import qualified Data.Text.Short               as ST

import           Data.PUS.CalibrationTypes
import           Data.TM.LogarithmicCalibration
import           Data.TM.NumericalCalibration
import           Data.TM.PolynomialCalibration
import           Data.TM.TextualCalibration

import           Text.Builder                  as TB


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

calibBuilder :: Calibration -> Int -> TB.Builder
calibBuilder (CalibNum  calib) indent = numericalCalibrationBuilder calib indent
calibBuilder (CalibText calib) indent = textualCalibrationBuilder calib indent
calibBuilder (CalibPoly calib) indent =
    polynomialCalibrationBuilder calib indent
calibBuilder (CalibLog calib) indent =
    logarithmicCalibrationBuilder calib indent


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
data CritCalib = CritCalib
    { _ccRLChk       :: !ShortText
    , _ccValPar      :: !Int
    , _ccCalibration :: Calibration
    }
    deriving (Show, Generic)
makeLenses ''CritCalib

instance NFData CritCalib
instance Serialise CritCalib
instance FromJSON CritCalib
instance ToJSON CritCalib where
    toEncoding = genericToEncoding defaultOptions

critCalibBuilder :: CritCalib -> Int -> TB.Builder
critCalibBuilder cr indent =
    padBuilder indent
        <> padFromRight 23 ' ' (text "<b>Applicable Param:</b>")
        <> text (ST.toText (_ccRLChk cr))
        <> newLineBuilder indent
        <> padFromRight 23 ' ' (text "<b>Value:</b>")
        <> decimal (_ccValPar cr)
        <> text " (0x"
        <> hexadecimal (_ccValPar cr)
        <> char ')'
        <> newLineBuilder indent
        <> "<b>Calibration:</b>\n"
        <> calibBuilder (_ccCalibration cr) (indent + 4)

padBuilder :: Int -> TB.Builder
padBuilder n = text (T.replicate n " ")

newLineBuilder :: Int -> TB.Builder
newLineBuilder n = char '\n' <> padBuilder n


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


calibContainerBuilder :: CalibContainer -> Int -> TB.Builder
calibContainerBuilder CritNoCalib indent =
    text (T.replicate indent " ") <> text "--"
calibContainerBuilder (CritDirect calib) indent = calibBuilder calib indent
calibContainerBuilder (Crit vec) indent =
    padBuilder indent <> text "<b>Applicability Criteria:</b>\n" <> vectorBuilder
  where
    vectorBuilder =
        TB.intercalate (char '\n')
            . map (\x -> critCalibBuilder x indent)
            . V.toList $ vec
