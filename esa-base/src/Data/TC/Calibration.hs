module Data.TC.Calibration
    ( TCCalibration(..)
    , TCNumericCalibration(..)
    , NumCalibVal(..)
    , TextCalibVal(..)
    , TCTextCalibration(..)
    ) where

import           RIO
import           Data.Text.Short                ( ShortText )
import           Data.TM.Value


data TCCalibration = 
  TCNumCalib TCNumericCalibration
  | TCTextCalib TCTextCalibration



data NumCalibVal = NumCalibVal
    { _ncvRaw :: TMValueSimple
    , _ncvEng :: TMValueSimple
    }

data TCNumericCalibration = TCNumericCalibration
    { _tcncName   :: !ShortText
    , _tcncDescr  :: !ShortText
    , _tcncUnit   :: !ShortText
    , _tcncValues :: Vector NumCalibVal
    }


data TextCalibVal = TextCalibVal
    { _tcvRaw :: TMValueSimple
    , _tcvEng  :: !ShortText
    }


data TCTextCalibration = TCTextCalibration
    { _tcvName   :: !ShortText
    , _tcvDescr  :: !ShortText
    , _tcvValues :: Vector TextCalibVal
    }
