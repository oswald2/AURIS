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

  )
where

import           RIO

import           Control.Lens                   ( makeLenses
                                                , (.~)
                                                )

import           Data.Text.Short                ( ShortText )

import           General.Types                  ( ToDouble(..) )

import           Data.TM.CalibrationTypes
import           Data.TM.Value
import           Data.TM.Validity        hiding ( isValid )




data LogarithmicCalibration = LogarithmicCalibration {
    _calibLName :: !ShortText
    , _calibLDescr :: !ShortText
    , _la0 :: !Double
    , _la1 :: !Double
    , _la2 :: !Double
    , _la3 :: !Double
    , _la4 :: !Double
    }
    deriving(Show)
makeLenses ''LogarithmicCalibration



instance Calibrate LogarithmicCalibration where
    calibrate calib rawValue | isValid rawValue = if isNumeric rawValue
        then
          let x = toDouble rawValue
          in  rawValue & tmvalValue .~ TMValDouble (interpolate calib x)
        else setValidity rawValue validitySetWrongType
    calibrate _calib rawValue = rawValue

interpolate :: LogarithmicCalibration -> Double -> Double
interpolate LogarithmicCalibration {..} x =
  let !lx  = log x
      !l2x = lx * lx
      !l3x = l2x * lx
      !l4x = l3x * lx
      !result = 1.0 / (_la0 + _la1 * lx + _la2 * l2x + _la3 * l3x + _la4 * l4x)
  in result 
