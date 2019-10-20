{-|
Module      : Data.TM.TextualCalibration
Description : Data type for a numerical calibration
Copyright   : (c) Michael Oswald, 2019
License     : BSD-3
Maintainer  : michael.oswald@onikudaki.net
Stability   : experimental
Portability : POSIX

This module handles textual calibrations of TM parameters
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
module Data.TM.TextualCalibration
  ( TextualCalibration(..)
  , TextCalibPoint(..)
  , txpLower
  , txpUpper
  , txpText
  )
where

import           RIO
import qualified RIO.Vector                    as V

import           Control.Lens                   ( makeLenses
                                                , (.~)
                                                )
import           Codec.Serialise
import           Data.Aeson

import           Data.Text.Short                ( ShortText )

import           Data.TM.CalibrationTypes
import           Data.TM.Value



-- | a calibration point for textual information. Textual
-- calibrations are only possible on integer values.
data TextCalibPoint = TextCalibPoint {
    -- | specifies the lower value of the range
    _txpLower :: !Int64
    -- | specified the upper value of the range
    , _txpUpper :: !Int64
    -- | specifies to text to be used when the parameter
    -- value falls within the range [lower, upper] (inclusive)
    , _txpText :: !ShortText
    }
    deriving (Show, Generic)
makeLenses ''TextCalibPoint

instance NFData TextCalibPoint
instance Serialise TextCalibPoint
instance FromJSON TextCalibPoint
instance ToJSON TextCalibPoint where
  toEncoding = genericToEncoding defaultOptions


data TextualCalibration = TextualCalibration {
        _calibTName :: !ShortText
        , _calibTDescr :: !ShortText
        , _calibTRawFmt :: !NumType
        , _calibTPoints :: Vector TextCalibPoint
    }
    deriving (Show, Generic)

instance NFData TextualCalibration
instance Serialise TextualCalibration
instance FromJSON TextualCalibration
instance ToJSON TextualCalibration where
  toEncoding = genericToEncoding defaultOptions


instance Calibrate TextualCalibration where
  calibrate TextualCalibration {..} rawValue | isValid rawValue =
    case _tmvalValue rawValue of
      TMValInt    x -> findValue _calibTPoints x
      TMValUInt   x -> findValue _calibTPoints x
      TMValDouble x -> findValue _calibTPoints (round x :: Int64)
      _             -> rawValue
   where
    findValue :: (Integral a) => Vector TextCalibPoint -> a -> TMValue
    findValue vec x = case V.find (isInRange x) vec of
      Nothing    -> rawValue
      Just point -> rawValue & tmvalValue .~ TMValString (_txpText point)
    isInRange :: (Integral a) => a -> TextCalibPoint -> Bool
    isInRange x TextCalibPoint {..} =
      _txpLower <= fromIntegral x && fromIntegral x <= _txpUpper
  calibrate _calib rawValue = rawValue
