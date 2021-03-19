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


data TCParamType =
    TCParamNormal
    | TCParamCmdID
    | TCParamParamID

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
    , _tcoObtID        :: !Int
    }


data ElemType =
    ElemFixedArea
    | ElemFixed
    | ElemEditable

data ElemFlag = 
    ElemRaw 
    | ElemEng
    | ElemCPC 
    | ElemTM


data TCParameterLocDef = TCParameterLocDef
    { _tcplElemType :: !ElemType
    , _tcplDescr :: !ShortText 
    , _tcplLen :: !BitSize 
    , _tcplBit :: !BitOffset 
    , _tcplGroupSize :: !Word16
    , _tcplElemFlag :: !ElemFlag
    , _tcplDefaultValue :: !TCParamDefaultValue
    , _tcplTMParam :: !ShortText
    , _tcplParam :: !TCParameterDef
    }
