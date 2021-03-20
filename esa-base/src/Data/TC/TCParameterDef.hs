module Data.TC.TCParameterDef
    ( TCParameterDef(..)
    , TCParamDefaultValue(..)
    , TCParamType(..)
    , TCParameterLocDef(..)
    , ElemType(..)
    , ElemFlag(..)
    ) where

import           RIO
import           Data.Text.Short                ( ShortText )

import           Codec.Serialise
import           Data.Aeson              hiding ( Value )

import           Data.PUS.Value

import           General.Types
import           Data.TC.RangeSet
import           Data.TC.Calibration

import           Data.TM.Value


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



data TCParamType =
    TCParamNormal
    | TCParamCmdID
    | TCParamParamID
    deriving(Show, Generic)

instance Serialise TCParamType
instance FromJSON TCParamType
instance ToJSON TCParamType where
    toEncoding = genericToEncoding defaultOptions



data TCParameterDef = TCParameterDef
    { _tcpName         :: !ShortText
    , _tcpDescr        :: !ShortText
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

instance Serialise TCParameterDef
instance FromJSON TCParameterDef
instance ToJSON TCParameterDef where
    toEncoding = genericToEncoding defaultOptions



data ElemType =
    ElemFixedArea
    | ElemFixed
    | ElemEditable
    deriving(Show, Generic)

instance Serialise ElemType
instance FromJSON ElemType
instance ToJSON ElemType where
    toEncoding = genericToEncoding defaultOptions


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


instance Serialise TCParameterLocDef
instance FromJSON TCParameterLocDef
instance ToJSON TCParameterLocDef where
    toEncoding = genericToEncoding defaultOptions
