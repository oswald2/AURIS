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
    , compareTCDefName
    ) where

import           RIO

import           Control.Lens                   ( makeLenses )
import           Data.Text.Short                ( ShortText )
import qualified Data.Text.Short               as ST

import           Codec.Serialise
import           Data.Aeson
import           General.APID
import           General.PUSTypes

import           Data.TC.TCParameterDef

import           Text.Builder                  as TB


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

instance Display TCType where
    textDisplay TCControlSegment = "CONTROL SEGMENT"
    textDisplay TCControlFrame   = "CONTROL FRAME"
    textDisplay TCNoCRC          = "NO CRC"
    textDisplay TCSleThrowEvent  = "SLE THROW"
    textDisplay TCNisThrowEvent  = "NIS THROW"
    textDisplay TCNormal         = "NORMAL"


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

instance Display InterlockScope where
    textDisplay ILGlobal          = "GLOBAL"
    textDisplay ILLocal           = "LOCAL"
    textDisplay ILSubSystem       = "SUB-SYSTEM"
    textDisplay ILGlobalSubsystem = "GLOBAL SUB-SYSTEM"
    textDisplay ILNone            = "NONE"



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

instance Display InterlockStage where
    textDisplay ILRelease          = "RELEASE"
    textDisplay ILUplink           = "UPLINK"
    textDisplay ILOnboardReception = "ONBOARD RECEPTION"
    textDisplay ILAcceptance       = "ACCEPTANCE"
    textDisplay ILCompletion       = "COMPLETION"


data ParamSet = ParamSet
    deriving (Show, Generic)

instance Serialise ParamSet
instance FromJSON ParamSet
instance ToJSON ParamSet where
    toEncoding = genericToEncoding defaultOptions

instance Display ParamSet where
    textDisplay ParamSet = ""


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

instance Display VerificationDef where
    textDisplay VerStageNone  = "NONE"
    textDisplay VerStageA     = "ACCEPTANCE"
    textDisplay VerStageS     = "START"
    textDisplay (VerStageP x) = "STEP " <> textDisplay x
    textDisplay VerStageC     = "COMPLETION"



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

instance Display TCDef where
    textDisplay = run . tcDefBuilder


tcDefBuilder :: TCDef -> TB.Builder
tcDefBuilder def =
    pad (text "<b>Name:</b> ")
        <> text (ST.toText (_tcDefName def))
        <> char '\n'
        <> pad (text "<b>Description:</b> ")
        <> text (ST.toText (_tcDefDescr def))
        <> char '\n'
        <> pad (text "<b>Description2:</b> ")
        <> text (ST.toText (_tcDefDescr2 def))
        <> char '\n'
        <> pad (text "<b>APID:</b> ")
        <> (maybe (text "--") (text . textDisplay) (_tcDefApid def))
    where pad b = padFromRight 23 ' ' b

compareTCDefName :: TCDef -> TCDef -> Ordering
compareTCDefName tc1 tc2 = compare (_tcDefName tc1) (_tcDefName tc2)


instance Serialise TCDef
instance FromJSON TCDef
instance ToJSON TCDef where
    toEncoding = genericToEncoding defaultOptions
