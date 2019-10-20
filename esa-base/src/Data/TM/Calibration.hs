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


data CalibContainer =
    CritNoCalib
    | CritDirect Calibration
    | Crit (Vector CritCalib)
    deriving (Show, Generic)

instance NFData CalibContainer
instance Serialise CalibContainer
instance FromJSON CalibContainer
instance ToJSON CalibContainer where
    toEncoding = genericToEncoding defaultOptions
