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
    , textualCalibrationBuilder
    ) where

import           RIO                     hiding ( (.~) )
import qualified RIO.Text                      as T
import qualified RIO.Vector                    as V

import           Codec.Serialise
import           Control.Lens                   ( (.~)
                                                , makeLenses
                                                )
import           Data.Aeson

import           Data.Text.Short                ( ShortText )
import qualified Data.Text.Short               as ST

import           Data.PUS.CalibrationTypes
import           Data.TM.Value

import           General.Types                  ( indentBuilder
                                                , newLineIndentBuilder
                                                , padRight
                                                )

import           Text.Builder                  as TB

-- | a calibration point for textual information. Textual
-- calibrations are only possible on integer values.
data TextCalibPoint = TextCalibPoint
    {
    -- | specifies the lower value of the range
      _txpLower :: !Int64
    -- | specified the upper value of the range
    , _txpUpper :: !Int64
    -- | specifies to text to be used when the parameter
    -- value falls within the range [lower, upper] (inclusive)
    , _txpText  :: !ShortText
    }
    deriving (Eq, Show, Generic)
makeLenses ''TextCalibPoint

instance NFData TextCalibPoint
instance Serialise TextCalibPoint
instance FromJSON TextCalibPoint
instance ToJSON TextCalibPoint where
    toEncoding = genericToEncoding defaultOptions

textCalibPointBuilder :: TextCalibPoint -> TB.Builder
textCalibPointBuilder pnt =
    text "Lower: "
        <> decimal (_txpLower pnt)
        <> ", Upper: "
        <> decimal (_txpUpper pnt)
        <> ", Text: "
        <> text (ST.toText (_txpText pnt))


data TextualCalibration = TextualCalibration
    { _calibTName   :: !ShortText
    , _calibTDescr  :: !ShortText
    , _calibTRawFmt :: !NumType
    , _calibTPoints :: Vector TextCalibPoint
    }
    deriving (Eq, Show, Generic)

instance NFData TextualCalibration
instance Serialise TextualCalibration
instance FromJSON TextualCalibration
instance ToJSON TextualCalibration where
    toEncoding = genericToEncoding defaultOptions

textualCalibrationBuilder :: TextualCalibration -> Word16 -> TB.Builder
textualCalibrationBuilder calib indent =
    indentBuilder indent
        <> text "<b>Name:</b> "
        <> text (ST.toText (_calibTName calib))
        <> newLineIndentBuilder indent
                                (padRight 23 (text "<b>Description:</b> "))
        <> text (ST.toText (_calibTDescr calib))
        <> newLineIndentBuilder indent
                                (padRight 23 (text "<b>Raw Format:</b> "))
        <> text (textDisplay (_calibTRawFmt calib))
        <> newLineIndentBuilder indent (text "<b>Points:</b>")
        <> newLineIndentBuilder indent (text "<b>-------</b>\n")
        <> pointBuilder (_calibTPoints calib) (indent + 4)


pointBuilder :: Vector TextCalibPoint -> Word16 -> TB.Builder
pointBuilder v indent = indentBuilder indent <> TB.intercalate
    (char '\n' <> indentBuilder indent)
    (RIO.map textCalibPointBuilder (V.toList v))


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
