{-# LANGUAGE TemplateHaskell #-}
module Verification.Verification
    ( Verification(..)
    , ReleaseStage(..)
    , GroundStage(..)
    , TMStage(..)
    , emptyVerification
    , defaultVerificationBD
    , defaultVerificationAD
    , setReleaseStage
    , setAllGroundStages
    , setAllTMStages
    , verRelease
    , verGroundReception
    , verGroundTransmission
    , verGroundOBR
    , verTMAcceptance
    , verTMStart
    , verTMProgress
    , verTMComplete
    , isFailed
    , isTMExpected
    , isGroundSuccess
    , isSuccess
    ) where

import           RIO
import qualified RIO.Vector                    as V

import           Control.Lens                   ( makeLenses
                                                , traversed
                                                )

import           Data.Aeson
import           Codec.Serialise

data ReleaseStage =
  StRDisabled
  | StRFail
  | StRSuccess
  deriving(Ord, Enum, Eq, Read, Show, Generic)

instance NFData ReleaseStage
instance Serialise ReleaseStage
instance FromJSON ReleaseStage
instance ToJSON ReleaseStage where
    toEncoding = genericToEncoding defaultOptions


instance Display ReleaseStage where
    textDisplay StRDisabled = " "
    textDisplay StRFail     = "F"
    textDisplay StRSuccess  = "S"




data GroundStage =
  StGDisabled
  | StGExpected
  | StGPending
  | StGTimeout
  | StGFail
  | StGAssumed
  | StGSuccess
  deriving(Ord, Enum, Eq, Read, Show, Generic)

instance NFData GroundStage
instance Serialise GroundStage
instance FromJSON GroundStage
instance ToJSON GroundStage where
    toEncoding = genericToEncoding defaultOptions

instance Display GroundStage where
    textDisplay StGDisabled = " "
    textDisplay StGExpected = " "
    textDisplay StGFail     = "F"
    textDisplay StGSuccess  = "S"
    textDisplay StGPending  = "P"
    textDisplay StGTimeout  = "T"
    textDisplay StGAssumed  = "A"


data TMStage =
  StTmDisabled
  | StTmExpected
  | StTmPending
  | StTmTimeout
  | StTmFail
  | StTmAssumed
  | StTmSuccess
  deriving(Ord, Enum, Eq, Read, Show, Generic)

instance NFData TMStage
instance Serialise TMStage
instance FromJSON TMStage
instance ToJSON TMStage where
    toEncoding = genericToEncoding defaultOptions


instance Display TMStage where
    textDisplay StTmDisabled = " "
    textDisplay StTmExpected = " "
    textDisplay StTmFail     = "F"
    textDisplay StTmSuccess  = "S"
    textDisplay StTmPending  = "P"
    textDisplay StTmTimeout  = "T"
    textDisplay StTmAssumed  = "A"


data Verification = Verification
    { _verRelease            :: ReleaseStage
    , _verGroundReception    :: GroundStage
    , _verGroundTransmission :: GroundStage
    , _verGroundOBR          :: GroundStage
    , _verTMAcceptance       :: TMStage
    , _verTMStart            :: TMStage
    , _verTMProgress         :: Vector TMStage
    , _verTMComplete         :: TMStage
    }
    deriving (Read, Show, Generic)
makeLenses ''Verification

instance NFData Verification
instance Serialise Verification
instance FromJSON Verification
instance ToJSON Verification where
    toEncoding = genericToEncoding defaultOptions

instance Display Verification where
    display Verification {..} =
        display _verRelease
            <> display @Text " "
            <> display _verGroundReception
            <> display _verGroundTransmission
            <> display _verGroundOBR
            <> display @Text " "
            <> display _verTMAcceptance
            <> display @Text " "
            <> display _verTMStart
            <> display @Text " "
            <> mconcat (map display (toList _verTMProgress))
            <> display @Text " "
            <> display _verTMComplete



emptyVerification :: Verification
emptyVerification = Verification { _verRelease            = StRDisabled
                                 , _verGroundReception    = StGDisabled
                                 , _verGroundTransmission = StGDisabled
                                 , _verGroundOBR          = StGDisabled
                                 , _verTMAcceptance       = StTmDisabled
                                 , _verTMStart            = StTmDisabled
                                 , _verTMProgress         = V.empty
                                 , _verTMComplete         = StTmDisabled
                                 }

defaultVerificationBD :: Verification
defaultVerificationBD = Verification { _verRelease            = StRDisabled
                                     , _verGroundReception    = StGExpected
                                     , _verGroundTransmission = StGExpected
                                     , _verGroundOBR          = StGDisabled
                                     , _verTMAcceptance       = StTmExpected
                                     , _verTMStart            = StTmExpected
                                     , _verTMProgress         = V.empty
                                     , _verTMComplete         = StTmExpected
                                     }

defaultVerificationAD :: Verification
defaultVerificationAD = defaultVerificationBD & verGroundOBR .~ StGExpected



setReleaseStage :: ReleaseStage -> Verification -> Verification
setReleaseStage StRFail verif =
    verif
        &  verRelease
        .~ StRFail
        &  setAllGroundStages StGDisabled
        &  setAllTMStages StTmDisabled
setReleaseStage status verif = verif & verRelease .~ status


setAllGroundStages :: GroundStage -> Verification -> Verification
setAllGroundStages status verif =
    verif
        &  verGroundReception
        .~ status
        &  verGroundTransmission
        .~ status
        &  verGroundOBR
        .~ status

setAllTMStages :: TMStage -> Verification -> Verification
setAllTMStages status verif =
    verif
        &  verTMAcceptance
        .~ status
        &  verTMStart
        .~ status
        &  verTMComplete
        .~ status
        &  verTMProgress
        .  traversed
        .~ status



isFailed :: Verification -> Bool
isFailed Verification {..} =
    _verRelease
        == StRFail
        || _verGroundReception
        == StGFail
        || _verGroundTransmission
        == StGFail
        || _verGroundOBR
        == StGFail
        || _verTMAcceptance
        == StTmFail
        || _verTMStart
        == StTmFail
        || _verTMComplete
        == StTmFail
        || V.any (== StTmFail) _verTMProgress


isTMExpected :: Verification -> Bool
isTMExpected Verification {..} =
    _verTMAcceptance
        == StTmExpected
        || _verTMStart
        == StTmExpected
        || _verTMComplete
        == StTmExpected
  -- we don't check for the progess, because for progess we need at 
  -- least a completion anyway


isGroundSuccess :: Verification -> Bool
isGroundSuccess Verification {..} =
    _verGroundOBR
        == StGSuccess
        || _verGroundOBR
        == StGDisabled
        && _verGroundTransmission
        == StGSuccess



isSuccess :: Verification -> Bool
isSuccess verif@Verification {..} =
    _verTMComplete
        == StTmSuccess
        || not (isTMExpected verif)
        && isGroundSuccess verif
