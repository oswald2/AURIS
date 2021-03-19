module Data.TC.Calibration
    ( TCCalibration(..)
    , TCNumericCalibration(..)
    , NumCalibVal(..)
    , TextCalibVal(..)
    , TCTextCalibration(..)
    ) where

import           RIO                            ( )
import           Data.Text.Short                ( ShortText )
import           Data.TM.Value

import           Data.PUS.CalibrationTypes
import           Data.Bimap


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
    , _tcncValues :: Bimap TMValueSimple TMValueSimple
    }

instance TcCalibration TCNumericCalibration TMValueSimple TMValueSimple where
    tcCalibrate TCNumericCalibration {..} from =
        Data.Bimap.lookup from _tcncValues
    tcDeCalibrate TCNumericCalibration {..} to' =
        Data.Bimap.lookupR to' _tcncValues



data TextCalibVal = TextCalibVal
    { _tcvRaw :: TMValueSimple
    , _tcvEng :: !ShortText
    }


data TCTextCalibration = TCTextCalibration
    { _tcvName   :: !ShortText
    , _tcvDescr  :: !ShortText
    , _tcvValues :: Bimap TMValueSimple ShortText
    }


instance TcCalibration TCTextCalibration TMValueSimple ShortText where
    tcCalibrate TCTextCalibration {..} from = Data.Bimap.lookup from _tcvValues
    tcDeCalibrate TCTextCalibration {..} to' =
        Data.Bimap.lookupR to' _tcvValues


