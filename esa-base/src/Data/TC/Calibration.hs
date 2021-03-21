module Data.TC.Calibration
    ( TCCalibration(..)
    , TCNumericCalibration(..)
    , TCTextCalibration(..)
    ) where

import           RIO
import           Data.Text.Short                ( ShortText )

import           Codec.Serialise
import           Data.Aeson              hiding ( Value )

import           Data.TM.Value

import           Data.PUS.CalibrationTypes
--import           Data.PUS.Value

import           Data.Bimap


data TCCalibration =
  TCNumCalib TCNumericCalibration
  | TCTextCalib TCTextCalibration
    deriving(Show, Generic)

instance Serialise TCCalibration
instance FromJSON TCCalibration
instance ToJSON TCCalibration where
    toEncoding = genericToEncoding defaultOptions


data TCNumericCalibration = TCNumericCalibration
    { _tcncName   :: !ShortText
    , _tcncDescr  :: !ShortText
    , _tcncUnit   :: !ShortText
    , _tcncValues :: Bimap TMValueSimple TMValueSimple
    }
    deriving (Show, Generic)

instance Serialise TCNumericCalibration
instance FromJSON TCNumericCalibration
instance ToJSON TCNumericCalibration where
    toEncoding = genericToEncoding defaultOptions


instance TcCalibration TCNumericCalibration TMValueSimple TMValueSimple where
    tcCalibrate TCNumericCalibration {..} from =
        Data.Bimap.lookup from _tcncValues
    tcDeCalibrate TCNumericCalibration {..} to' =
        Data.Bimap.lookupR to' _tcncValues



data TCTextCalibration = TCTextCalibration
    { _tcvName   :: !ShortText
    , _tcvDescr  :: !ShortText
    , _tcvValues :: Bimap TMValueSimple ShortText
    }
    deriving (Show, Generic)

instance Serialise TCTextCalibration
instance FromJSON TCTextCalibration
instance ToJSON TCTextCalibration where
    toEncoding = genericToEncoding defaultOptions



instance TcCalibration TCTextCalibration TMValueSimple ShortText where
    tcCalibrate TCTextCalibration {..} from = Data.Bimap.lookup from _tcvValues
    tcDeCalibrate TCTextCalibration {..} to' =
        Data.Bimap.lookupR to' _tcvValues


