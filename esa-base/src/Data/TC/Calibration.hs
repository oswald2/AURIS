module Data.TC.Calibration
    ( TCCalibration(..)
    , TCNumericCalibration(..)
    , TCTextCalibration(..)
    , tcNumericCalibrationBuilder
    , tcTextCalibrationBuilder
    , tcCalibBuilder
    ) where

import           RIO

import           Data.Text.Short                ( ShortText )
import qualified Data.Text.Short               as ST

import           Codec.Serialise
import           Data.Aeson              hiding ( Value )

import           Data.TM.Value

import           Data.PUS.CalibrationTypes

import           Data.Bimap

import           Text.Builder                  as TB

import           General.Types



data TCCalibration =
  TCNumCalib TCNumericCalibration
  | TCTextCalib TCTextCalibration
    deriving(Show, Generic)

instance Serialise TCCalibration
instance FromJSON TCCalibration
instance ToJSON TCCalibration where
    toEncoding = genericToEncoding defaultOptions

tcCalibBuilder :: Word16 -> TCCalibration -> TB.Builder
tcCalibBuilder indent (TCNumCalib  c) = tcNumericCalibrationBuilder indent c
tcCalibBuilder indent (TCTextCalib c) = tcTextCalibrationBuilder indent c


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

tcNumericCalibrationBuilder :: Word16 -> TCNumericCalibration -> TB.Builder
tcNumericCalibrationBuilder indent cal =
    indentBuilder indent
        <> padRight 23 (text "<b>Name:</b> ")
        <> text (ST.toText (_tcncName cal))
        <> newLineIndentBuilder indent (padRight 23 (text "<b>Description:</b> "))
        <> text (ST.toText (_tcncDescr cal))
        <> newLineIndentBuilder indent (padRight 23 (text "<b>Unit:</b> "))
        <> text (ST.toText (_tcncUnit cal))
        <> newLineIndentBuilder indent       (text "<b>Values:</b>\n")
        <> newLineIndentBuilder (indent + 4) (valueBuilder (indent + 4))
  where
    valueBuilder ind =
        TB.intercalate (char '\n' <> indentBuilder ind)
            . RIO.map f
            . Data.Bimap.toList
            $ _tcncValues cal
    f (v1, v2) = padFromLeft 16 ' ' (text (textDisplay v1)) <> text "    " <> text (textDisplay v2)



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

tcTextCalibrationBuilder :: Word16 -> TCTextCalibration -> TB.Builder
tcTextCalibrationBuilder indent cal =
    indentBuilder indent <> padRight 23 (text "<b>Name:</b> ")
        <> text (ST.toText (_tcvName cal))
        <> newLineIndentBuilder indent (padRight 23 (text "<b>Description:</b> "))
        <> text (ST.toText (_tcvDescr cal))
        <> newLineIndentBuilder indent       (text "<b>Values:</b>\n")
        <> newLineIndentBuilder (indent + 4) (valueBuilder (indent + 4))
  where
    valueBuilder ind =
        TB.intercalate (char '\n' <> indentBuilder ind)
            . RIO.map f
            . Data.Bimap.toList
            $ _tcvValues cal
    f (v1, v2) = padFromLeft 16 ' ' (text (textDisplay v1)) <> text "    " <> text (ST.toText v2)




instance TcCalibration TCTextCalibration TMValueSimple ShortText where
    tcCalibrate TCTextCalibration {..} from = Data.Bimap.lookup from _tcvValues
    tcDeCalibrate TCTextCalibration {..} to' =
        Data.Bimap.lookupR to' _tcvValues


