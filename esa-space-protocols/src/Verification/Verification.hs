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



data GroundStage =
  StGDisabled
  | StGExpected
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



data TMStage =
  StTmDisabled
  | StTmExpected
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
