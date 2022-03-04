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
import qualified RIO.Vector                    as V

import           Control.Lens                   ( makeLenses )

import           Data.Text.Short                ( ShortText )
import qualified Data.Text.Short               as ST

import           Codec.Serialise

import           Data.Aeson

import           General.APID
import           General.PUSTypes
import           General.Types

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
    textDisplay VerStageNone  = ""
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
    padRight 23 (text "<b>Name:</b> ")
        <> text (ST.toText (_tcDefName def))
        <> char '\n'
        <> padRight 23 (text "<b>Description:</b> ")
        <> text (ST.toText (_tcDefDescr def))
        <> char '\n'
        <> padRight 23 (text "<b>Description2:</b> ")
        <> text (ST.toText (_tcDefDescr2 def))
        <> char '\n'
        <> padRight 23 (text "<b>Type:</b> ")
        <> text (textDisplay (_tcDefCType def))
        <> char '\n'
        <> padRight 23 (text "<b>Critical:</b> ")
        <> (if _tcDefCritical def then text "YES" else text "NO")
        <> char '\n'
        <> padRight 23 (text "<b>APID:</b> ")
        <> (maybe (text "--") (text . textDisplay) (_tcDefApid def))
        <> char '\n'
        <> padRight 23 (text "<b>PUS Type:</b> ")
        <> pusTypes
        <> char '\n'
        <> padRight 23 (text "<b>Loaded:</b> ")
        <> (if _tcDefExec def then text "NO RESTRICTION" else text "SEQUENCE")
        <> char '\n'
        <> padRight 23 (text "<b>Interlock Scope:</b> ")
        <> text (textDisplay (_tcDefILScope def))
        <> char '\n'
        <> padRight 23 (text "<b>Interlock Stage:</b> ")
        <> text (textDisplay (_tcDefILStage def))
        <> char '\n'
        <> padRight 23 (text "<b>Subsystem:</b> ")
        <> maybe (text "--") decimal (_tcDefSubSys def)
        <> char '\n'
        <> padRight 23 (text "<b>MAPID:</b> ")
        <> maybe (text "--") (text . textDisplay) (_tcDefMapID def)
        <> char '\n'
        <> padRight 23 (text "<b>Parameter Set:</b> ")
        <> text (textDisplay (_tcDefParamSet def))
        <> char '\n'
        <> padRight 23 (text "<b>Ack Flags:</b> ")
        <> text "0x"
        <> hexadecimal (_tcDefAckFlags def)
        <> char '\n'
        <> padRight 23 (text "<b>Subschedule:</b> ")
        <> maybe (text "--") decimal (_tcDefSubSched def)
        <> char '\n'
        <> padRight 23 (text "<b>Verif Stages:</b> ")
        <> verifStages
        <> char '\n'
        <> padRight 23 (text "<b>Connection:</b> ")
        <> text (ST.toText (_tcDefConnection def))
        <> char '\n'
        <> padRight 23 (text "<b>Conn Flag:</b> ")
        <> text (textDisplay (_tcDefConnectionFlag def))
        <> char '\n'
        <> text "<b>Parameters:</b>\n"
        <> params

  where
    pusTypes = case (_tcDefType def, _tcDefSubType def) of
        (Just t, Just st) ->
            char '('
                <> text (textDisplay t)
                <> text ", "
                <> text (textDisplay st)
                <> char ')'
        _ -> text "--"
    verifStages =
        TB.intercalate (text ", ")
            . map (text . textDisplay)
            . V.toList
            . _tcDefVerifStages
            $ def

    params =
        TB.intercalate (char '\n')
            . map (tcParameterLocDefBuilder 4)
            . V.toList
            . _tcDefParams
            $ def



compareTCDefName :: TCDef -> TCDef -> Ordering
compareTCDefName tc1 tc2 = compare (_tcDefName tc1) (_tcDefName tc2)


instance Serialise TCDef
instance FromJSON TCDef
instance ToJSON TCDef where
    toEncoding = genericToEncoding defaultOptions
