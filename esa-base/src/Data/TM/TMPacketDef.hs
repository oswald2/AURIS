{-# LANGUAGE
    TemplateHaskell
#-}
module Data.TM.TMPacketDef
    ( TMPacketDef(..)
    , SuperCommutated(..)
    , TMParamLocation(..)
    , TMPacketParams(..)
    , PIDEvent(..)
    , tmpdSPID
    , tmpdType
    , tmpdSubType
    , tmpdApid
    , tmpdPI1Val
    , tmpdPI2Val
    , tmpdName
    , tmpdParams
    , tmpdDescr
    , tmpdUnit
    , tmpdTime
    , tmpdInter
    , tmpdValid
    , tmpdCheck
    , tmpdEvent
    , scNbOcc
    , scLgOcc
    , scTdOcc
    , tmplName
    , tmplOffset
    , tmplTime
    , tmplSuperComm
    , tmplParam
    , isSuperCommutated
    )
where

import           RIO
import           Data.Text.Short                ( ShortText )

import           Control.Lens                   ( makeLenses )

import           General.PUSTypes
import           General.APID
import           General.Types
import           General.Time
import           General.TimeSpan

import           Data.TM.TMParameterDef

--import           Data.TM.PIVals



data SuperCommutated = SuperCommutated {
  _scNbOcc :: !Int
  , _scLgOcc :: !Int
  , _scTdOcc :: !Int
  } deriving (Show, Generic)
makeLenses ''SuperCommutated

data TMParamLocation = TMParamLocation {
  _tmplName :: !ShortText
  , _tmplOffset :: !BitOffset
  , _tmplTime :: !SunTime
  , _tmplSuperComm :: Maybe SuperCommutated
  , _tmplParam :: TMParameterDef
  } deriving (Show, Generic)
makeLenses ''TMParamLocation

isSuperCommutated :: TMParamLocation -> Bool
isSuperCommutated TMParamLocation {..} = isJust _tmplSuperComm

data PIDEvent =
  PIDNo
  | PIDInfo !ShortText
  | PIDWarning !ShortText
  | PIDAlarm !ShortText
  deriving (Eq, Ord, Show, Generic)


data TMPacketParams =
  TMFixedParams (Vector TMParamLocation)
  | TMVariableParams {
    _tmvpTPSD :: !Int
    , _tmvpDfhSize :: !Word8
  }
  deriving (Show, Generic)


data TMPacketDef = TMPacketDef {
    _tmpdSPID :: !SPID
    , _tmpdName :: !ShortText
    , _tmpdDescr :: !ShortText
    , _tmpdType :: !PUSType
    , _tmpdSubType :: !PUSSubType
    , _tmpdApid :: !APID
    , _tmpdPI1Val :: !Int
    , _tmpdPI2Val :: !Int
    , _tmpdUnit :: !ShortText
    , _tmpdTime :: !Bool
    , _tmpdInter :: Maybe (TimeSpn MilliSeconds)
    , _tmpdValid :: !Bool
    , _tmpdCheck :: !Bool
    , _tmpdEvent :: !PIDEvent
    , _tmpdParams :: TMPacketParams
    } deriving(Show, Generic)
makeLenses ''TMPacketDef


