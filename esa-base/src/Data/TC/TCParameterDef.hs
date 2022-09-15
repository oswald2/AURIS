{-# LANGUAGE TemplateHaskell #-}
module Data.TC.TCParameterDef
    ( TCParameterDef(..)
    , TCParamDefaultValue(..)
    , TCParamType(..)
    , TCParameterLocDef(..)
    , ElemType(..)
    , ElemFlag(..)
    , compareTCParameterDefName
    , tcpName
    , tcpDescr
    , tcpDefaultValue
    , tcpPTC
    , tcpPFC
    , tcpRadix
    , tcpUnit
    , tcpProcType
    , tcpCalib
    , tcpRange
    , tcpCorrelate
    , tcpObtID
    , tcplElemType
    , tcplDescr
    , tcplLen
    , tcplBit
    , tcplGroupSize
    , tcplElemFlag
    , tcplDefaultValue
    , tcplTMParam
    , tcplParam
    , tcParameterDefBuilder
    , tcParameterLocDefBuilder
    ) where

import           RIO

import           Data.Text.Short                ( ShortText )
import qualified Data.Text.Short               as ST

import           Control.Lens                   ( makeLenses )

import           Codec.Serialise
import           Data.Aeson              hiding ( Value )

import           Data.PUS.Value

import           Data.TC.Calibration
import           Data.TC.RangeSet

import           General.TextTools
import           General.Types

import           Data.TM.Value

import           General.PUSTypes

import           Text.Builder                  as TB


data TCParamDefaultValue =
    TCParamNothing
    | TCParamRaw Value
    | TCParamEng TMValueSimple
    | TCCmdID ShortText
    | TCParamID ShortText
    deriving(Show, Generic)

instance Serialise TCParamDefaultValue
instance FromJSON TCParamDefaultValue
instance ToJSON TCParamDefaultValue where
    toEncoding = genericToEncoding defaultOptions

tcParamDefaultValueBuilder :: TCParamDefaultValue -> TB.Builder
tcParamDefaultValueBuilder TCParamNothing   = "--"
tcParamDefaultValueBuilder (TCParamRaw val) = text (textDisplay val)
tcParamDefaultValueBuilder (TCParamEng val) = text (textDisplay val)
tcParamDefaultValueBuilder (TCCmdID    val) = text (ST.toText val)
tcParamDefaultValueBuilder (TCParamID  val) = text (ST.toText val)




data TCParamType =
    TCParamNormal
    | TCParamCmdID
    | TCParamParamID
    deriving(Show, Generic)

instance Serialise TCParamType
instance FromJSON TCParamType
instance ToJSON TCParamType where
    toEncoding = genericToEncoding defaultOptions

tcParamTypeBuilder :: TCParamType -> TB.Builder
tcParamTypeBuilder TCParamNormal  = text "NORMAL"
tcParamTypeBuilder TCParamCmdID   = text "COMMAND ID"
tcParamTypeBuilder TCParamParamID = text "PARAMETER ID"



data TCParameterDef = TCParameterDef
    { _tcpName         :: !ShortText
    , _tcpDescr        :: !ShortText
    , _tcpPTC          :: !PTC
    , _tcpPFC          :: !PFC
    , _tcpDefaultValue :: !TCParamDefaultValue
    , _tcpRadix        :: !Radix
    , _tcpUnit         :: !ShortText
    , _tcpProcType     :: !TCParamType
    , _tcpCalib        :: Maybe TCCalibration
    , _tcpRange        :: Maybe RangeSet
    , _tcpCorrelate    :: !Correlate
    , _tcpObtID        :: !Int
    }
    deriving (Show, Generic)
makeLenses ''TCParameterDef

instance Serialise TCParameterDef
instance FromJSON TCParameterDef
instance ToJSON TCParameterDef where
    toEncoding = genericToEncoding defaultOptions

compareTCParameterDefName :: TCParameterDef -> TCParameterDef -> Ordering
compareTCParameterDefName p1 p2 = compare (_tcpName p1) (_tcpName p2)

instance Display TCParameterDef where
    textDisplay x = run $ tcParameterDefBuilder 0 x


tcParameterDefBuilder :: Word16 -> TCParameterDef -> TB.Builder
tcParameterDefBuilder indent def =
    indentBuilder indent
        <> padRight 23 (text "<b>Name:</b> ")
        <> text (ST.toText (def ^. tcpName))
        <> newLineIndentBuilder indent
                                (padRight 23 (text "<b>Description:</b> "))
        <> escapeTextBuilder (ST.toText (def ^. tcpDescr))
        <> newLineIndentBuilder indent (padRight 23 (text "<b>PTC:</b> "))
        <> text (textDisplay (def ^. tcpPTC))
        <> newLineIndentBuilder indent (padRight 23 (text "<b>PFC:</b> "))
        <> text (textDisplay (def ^. tcpPFC))
        <> newLineIndentBuilder
               indent
               (padRight 23 (text "<b>Default Value:</b> "))
        <> tcParamDefaultValueBuilder (def ^. tcpDefaultValue)
        <> newLineIndentBuilder indent (padRight 23 (text "<b>Radix:</b> "))
        <> text (textDisplay (def ^. tcpRadix))
        <> newLineIndentBuilder indent (padRight 23 (text "<b>Unit:</b> "))
        <> text (ST.toText (def ^. tcpUnit))
        <> newLineIndentBuilder indent (padRight 23 (text "<b>Type:</b> "))
        <> tcParamTypeBuilder (def ^. tcpProcType)
        <> newLineIndentBuilder indent
                                (padRight 23 (text "<b>Calibration:</b> "))
        <> maybe (text "--")
                 (\x -> char '\n' <> tcCalibBuilder (indent + 4) x)
                 (def ^. tcpCalib)
        <> newLineIndentBuilder indent
                                (padRight 23 (text "<b>Range Set:</b>\n"))
        <> newLineIndentBuilder indent (padRight 23 (text "<b>Correlate:</b> "))
        <> text (textDisplay (def ^. tcpCorrelate))
        <> newLineIndentBuilder indent (padRight 23 (text "<b>OBT ID:</b> "))
        <> decimal (def ^. tcpObtID)


data ElemType =
    ElemFixedArea
    | ElemFixed
    | ElemEditable
    deriving(Show, Generic)

instance Serialise ElemType
instance FromJSON ElemType
instance ToJSON ElemType where
    toEncoding = genericToEncoding defaultOptions

elemTypeBuilder :: ElemType -> TB.Builder
elemTypeBuilder ElemFixedArea = "FIXED AREA"
elemTypeBuilder ElemFixed     = "FIXED"
elemTypeBuilder ElemEditable  = "EDITABLE"



data ElemFlag =
    ElemRaw
    | ElemEng
    | ElemCPC
    | ElemTM
    deriving(Show, Generic)

instance Serialise ElemFlag
instance FromJSON ElemFlag
instance ToJSON ElemFlag where
    toEncoding = genericToEncoding defaultOptions

elemFlagBuilder :: ElemFlag -> TB.Builder
elemFlagBuilder ElemRaw = "RAW"
elemFlagBuilder ElemEng = "ENG"
elemFlagBuilder ElemCPC = "CPC"
elemFlagBuilder ElemTM  = "TM"



data TCParameterLocDef = TCParameterLocDef
    { _tcplElemType     :: !ElemType
    , _tcplDescr        :: !ShortText
    , _tcplLen          :: !BitSize
    , _tcplBit          :: !BitOffset
    , _tcplGroupSize    :: !Word16
    , _tcplElemFlag     :: !ElemFlag
    , _tcplDefaultValue :: !TCParamDefaultValue
    , _tcplTMParam      :: !ShortText
    , _tcplParam        :: !TCParameterDef
    }
    deriving (Show, Generic)
makeLenses ''TCParameterLocDef


instance Serialise TCParameterLocDef
instance FromJSON TCParameterLocDef
instance ToJSON TCParameterLocDef where
    toEncoding = genericToEncoding defaultOptions


tcParameterLocDefBuilder :: Word16 -> TCParameterLocDef -> TB.Builder
tcParameterLocDefBuilder indent def =
    let newIndent = indent + 4
    in
        indentBuilder indent
        <> padRight 23 (text "<b>Name:</b> ")
        <> text (ST.toText (def ^. tcplParam . tcpName))
        <> newLineIndentBuilder newIndent (padRight 23 ("<b>Description</b>: "))
        <> escapeTextBuilder (ST.toText (def ^. tcplDescr))
        <> newLineIndentBuilder newIndent
                                (padRight 23 ("<b>Element Type</b>: "))
        <> elemTypeBuilder (def ^. tcplElemType)
        <> newLineIndentBuilder newIndent
                                (padRight 23 ("<b>Element Flag</b>: "))
        <> elemFlagBuilder (def ^. tcplElemFlag)
        <> newLineIndentBuilder newIndent (padRight 23 ("<b>Size</b>: "))
        <> text (textDisplay (def ^. tcplLen))
        <> newLineIndentBuilder newIndent (padRight 23 ("<b>Offset</b>: "))
        <> text (textDisplay (def ^. tcplBit))
        <> newLineIndentBuilder newIndent (padRight 23 ("<b>Group Size</b>: "))
        <> decimal (def ^. tcplGroupSize)
        <> newLineIndentBuilder newIndent
                                (padRight 23 ("<b>Default Value</b>: "))
        <> tcParamDefaultValueBuilder (def ^. tcplDefaultValue)
        <> newLineIndentBuilder newIndent
                                (padRight 23 ("<b>TM Parameter</b>: "))
        <> text (ST.toText (def ^. tcplTMParam))

