module Data.TC.TCParameterDef
    ( TCParameterDef(..)
    , TCParamDefaultValue(..)
    , TCParamType(..)
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



