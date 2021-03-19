module Data.TC.TCDef
    ( TCDef(..)
    , TCType(..)
    , InterlockScope(..)
    , InterlockStage(..)
    , ParamSet(..)
    ) where


import           RIO
import           Data.Text.Short                ( ShortText )
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


data InterlockScope =
  ILGlobal
  | ILLocal
  | ILSubSystem
  | ILGlobalSubsystem
  | ILNone

data InterlockStage =
  ILRelease
  | ILUplink
  | ILOnboardReception
  | ILAcceptance
  | ILCompletion


data ParamSet = ParamSet

data TCDef = TCDef
    { _tcDefName     :: !ShortText
    , _tcDefDescr    :: !ShortText
    , _tcDefDescr2   :: !ShortText
    , _tcDefCType    :: !TCType
    , _tcDefCritical :: !Bool
    , _tcDefApid     :: Maybe APID
    , _tcDefType     :: Maybe PUSType
    , _tcDefSubType  :: Maybe PUSSubType
    , _tcDefExec     :: !Bool
    , _tcDefILScope  :: !InterlockScope
    , _tcDefILStage  :: !InterlockStage
    , _tcDefSubSys   :: Maybe Int
    , _tcDefMapID    :: Maybe MAPID
    , _tcDefParamSet :: !ParamSet
    , _tcDefAckFlags :: !Word8
    , _tcDefSubSched :: Maybe Int
    , _tcDefParams   :: Vector TCParameterLocDef
    }
