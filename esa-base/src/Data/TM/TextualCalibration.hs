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

import           Control.Lens                   ( makeLenses )

import           Data.TM.CalibrationTypes


data TextCalibPoint = TextCalibPoint {
    _txpLower :: !Int64
    , _txpUpper :: !Int64
    , _txpText :: !Text
    }
makeLenses ''TextCalibPoint


data TextualCalibration = TextualCalibration



instance Calibrate TextualCalibration where
  calibrate _calib rawValue = rawValue
