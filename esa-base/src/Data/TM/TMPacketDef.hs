{-# LANGUAGE
    TemplateHaskell
#-}
module Data.TM.TMPacketDef
  ( TMPacketDef(..)
  , tmpdSPID
  , tmpdType
  , tmpdSubType
  , tmpdApid
  , tmpdPI1Val
  , tmpdPI2Val
  , tmpdName
  , tmpdParams

  , scNbOcc 
  , scLgOcc 
  , scTdOcc 
  , tmplName 
  , tmplOffset
  , tmplTime 
  , tmplSuperComm
  , tmplParam

  )
where

import           RIO
import           Data.Text.Short                ( ShortText )

import           Control.Lens                   ( makeLenses )

import           General.PUSTypes
import           General.APID
import           General.Types
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
  , _tmplTime :: !TimeSpan
  , _tmplSuperComm :: Maybe SuperCommutated
  , _tmplParam :: TMParameterDef
  } deriving (Show, Generic)
makeLenses ''TMParamLocation



data TMPacketParams =
  TMFixedParams (Vector TMParamLocation)
  | TMVariableParams
  deriving (Show, Generic)


data TMPacketDef = TMPacketDef {
    _tmpdSPID :: !SPID
    , _tmpdName :: !ShortText
    , _tmpdType :: !PUSType
    , _tmpdSubType :: !PUSSubType
    , _tmpdApid :: !APID
    , _tmpdPI1Val :: !Int
    , _tmpdPI2Val :: !Int
    , _tmpdParams :: TMPacketParams
    } deriving(Show, Generic)
makeLenses ''TMPacketDef


