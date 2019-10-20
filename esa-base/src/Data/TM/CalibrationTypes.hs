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
module Data.TM.CalibrationTypes
  ( Calibrate(..)
  , CalibInterpolation(..)
  , toCalibInterpolation
  )
where

import           RIO
import           Codec.Serialise
import           Data.Aeson

import           Data.TM.Value



data CalibInterpolation =
    CalibExtrapolate
    | CalibFail
    deriving (Eq, Ord, Enum, Show, Read, Generic)

instance NFData CalibInterpolation
instance Serialise CalibInterpolation
instance FromJSON CalibInterpolation
instance ToJSON CalibInterpolation where
  toEncoding = genericToEncoding defaultOptions


toCalibInterpolation :: Char -> CalibInterpolation
toCalibInterpolation 'P' = CalibExtrapolate
toCalibInterpolation _   = CalibFail


class Calibrate a where
    calibrate :: a -> TMValue -> TMValue

