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

import           Data.Text.Short                ( ShortText )

import           Data.TM.CalibrationTypes
import           Data.TM.Value

data TextCalibPoint = TextCalibPoint {
    _txpLower :: !Int64
    , _txpUpper :: !Int64
    , _txpText :: !ShortText
    }
    deriving (Show)
makeLenses ''TextCalibPoint


data TextualCalibration = TextualCalibration {
        _calibTName :: !ShortText
        , _calibTDescr :: !ShortText
        , _calibTRawFmt :: !NumType
        , _calibTPoints :: Vector TextCalibPoint
    }
    deriving (Show)



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
