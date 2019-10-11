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
  )
where

import           RIO

import           Control.Lens                   ( makePrisms )
import           Codec.Serialise
import           Data.Aeson

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

instance Serialise Calibration
instance FromJSON Calibration
instance ToJSON Calibration where
  toEncoding = genericToEncoding defaultOptions


instance Calibrate Calibration where
  calibrate (CalibNum  x) = calibrate x
  calibrate (CalibText x) = calibrate x
  calibrate (CalibPoly x) = calibrate x
  calibrate (CalibLog  x) = calibrate x
