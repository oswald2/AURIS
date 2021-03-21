{-# LANGUAGE TemplateHaskell #-}
module Data.TC.TCDef
    ( TCDef(..)
    , TCType(..)
    , InterlockScope(..)
    , InterlockStage(..)
    , ParamSet(..)
    , VerificationDef(..)
    , tcDefName
    , tcDefDescr
    , tcDefDescr2
    , tcDefCType
    , tcDefCritical
    , tcDefApid
    , tcDefType
    , tcDefSubType
    , tcDefExec
    , tcDefILScope
    , tcDefILStage
    , tcDefSubSys
    , tcDefMapID
    , tcDefParamSet
    , tcDefAckFlags
    , tcDefSubSched
    , tcDefConnection
    , tcDefConnectionFlag
    , tcDefVerifStages
    , tcDefParams
    ) where

import           RIO
import           Data.Text.Short                ( ShortText )
import           Control.Lens                   ( makeLenses )

import           Codec.Serialise
import           Data.Aeson
import           General.PUSTypes
import           General.APID

import           Data.TC.TCParameterDef

data TCType =
  TCControlSegment
  | TCControlFrame
  | TCNoCRC
  | TCSleThrowEvent
  | TCNisThrowEvent
  | TCNormal
  deriving (Eq, Ord, Enum, Show, Generic)

instance Serialise TCType
instance FromJSON TCType
instance ToJSON TCType where
    toEncoding = genericToEncoding defaultOptions


data InterlockScope =
  ILGlobal
  | ILLocal
  | ILSubSystem
  | ILGlobalSubsystem
  | ILNone
  deriving (Eq, Ord, Enum, Show, Generic)

instance Serialise InterlockScope
instance FromJSON InterlockScope
instance ToJSON InterlockScope where
    toEncoding = genericToEncoding defaultOptions


data InterlockStage =
  ILRelease
  | ILUplink
  | ILOnboardReception
  | ILAcceptance
  | ILCompletion
  deriving (Eq, Ord, Enum, Show, Generic)

instance Serialise InterlockStage
instance FromJSON InterlockStage
instance ToJSON InterlockStage where
    toEncoding = genericToEncoding defaultOptions



data ParamSet = ParamSet
    deriving (Show, Generic)

instance Serialise ParamSet
instance FromJSON ParamSet
instance ToJSON ParamSet where
    toEncoding = genericToEncoding defaultOptions




data VerificationDef =
  VerStageNone
  | VerStageA
  | VerStageS
  | VerStageP Int
  | VerStageC
  deriving (Eq, Ord, Show, Generic)

instance Serialise VerificationDef
instance FromJSON VerificationDef
instance ToJSON VerificationDef where
    toEncoding = genericToEncoding defaultOptions



data TCDef = TCDef
    { _tcDefName           :: !ShortText
    , _tcDefDescr          :: !ShortText
    , _tcDefDescr2         :: !ShortText
    , _tcDefCType          :: !TCType
    , _tcDefCritical       :: !Bool
    , _tcDefApid           :: Maybe APID
    , _tcDefType           :: Maybe PUSType
    , _tcDefSubType        :: Maybe PUSSubType
    , _tcDefExec           :: !Bool
    , _tcDefILScope        :: !InterlockScope
    , _tcDefILStage        :: !InterlockStage
    , _tcDefSubSys         :: Maybe Int
    , _tcDefMapID          :: Maybe MAPID
    , _tcDefParamSet       :: !ParamSet
    , _tcDefAckFlags       :: !Word8
    , _tcDefSubSched       :: Maybe Int
    , _tcDefVerifStages    :: Vector VerificationDef
    , _tcDefConnection     :: !ShortText
    , _tcDefConnectionFlag :: !CommandType
    , _tcDefParams         :: Vector TCParameterLocDef
    }
    deriving (Show, Generic)
makeLenses ''TCDef

instance Serialise TCDef
instance FromJSON TCDef
instance ToJSON TCDef where
    toEncoding = genericToEncoding defaultOptions
