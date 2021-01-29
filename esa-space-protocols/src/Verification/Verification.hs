{-# LANGUAGE TemplateHaskell #-}
module Verification.Verification
    ( Verification(..)
    , ReleaseStage(..)
    , GroundStage(..)
    , TMStage(..)
    , VerifStatus(..)
    , emptyVerification
    , defaultVerificationBD
    , defaultVerificationAD
    , defaultVerificationSCOE
    , setReleaseStage
    , setGroundReceptionStage
    , setGroundTransmissionStage
    , setGroundOBRStage
    , setGroundGTStages
    , setTMAcceptStage
    , setTMStartStage
    , setTMCompleteStage
    , setTMProgressStage
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
    , verStatus
    , isFailed
    , isTMExpected
    , isTMAExpected
    , isTMSExpected
    , isTMCExpected
    , isGroundSuccess
    , isGroundExpected
    , isGroundTimeout
    , isGroundFail
    , isGroundDisabled
    , isOBAExpected
    , isSuccess
    , isTimeout
    , isFinished
    ) where

import           RIO
import qualified RIO.Vector                    as V
import qualified RIO.Vector.Partial            as V
                                                ( (//) )

import           Control.Lens                   ( makeLenses
                                                , traversed
                                                )

import           Data.Aeson
import           Codec.Serialise

data ReleaseStage =
  StRDisabled
  | StRPending
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
    textDisplay StRPending  = "P"



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


data VerifStatus =
    VerifStatNominal
    | VerifStatDeleted
    | VerifStatEnabled
    | VerifStatDisabled
    deriving (Eq, Ord, Enum, Show, Read, Generic)

instance NFData VerifStatus
instance Serialise VerifStatus
instance FromJSON VerifStatus
instance ToJSON VerifStatus where
    toEncoding = genericToEncoding defaultOptions


instance Display VerifStatus where
    textDisplay VerifStatNominal  = ""
    textDisplay VerifStatDeleted  = "DELETED"
    textDisplay VerifStatEnabled  = "ENABLED"
    textDisplay VerifStatDisabled = "DISABLED"


data Verification = Verification
    { _verRelease            :: ReleaseStage
    , _verGroundReception    :: GroundStage
    , _verGroundTransmission :: GroundStage
    , _verGroundOBR          :: GroundStage
    , _verTMAcceptance       :: TMStage
    , _verTMStart            :: TMStage
    , _verTMProgress         :: Vector TMStage
    , _verTMComplete         :: TMStage
    , _verStatus             :: VerifStatus
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
                                 , _verStatus             = VerifStatNominal
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
                                     , _verStatus             = VerifStatNominal
                                     }

defaultVerificationAD :: Verification
defaultVerificationAD = defaultVerificationBD & verGroundOBR .~ StGExpected


defaultVerificationSCOE :: Verification
defaultVerificationSCOE = Verification { _verRelease            = StRDisabled
                                       , _verGroundReception    = StGExpected
                                       , _verGroundTransmission = StGExpected
                                       , _verGroundOBR          = StGDisabled
                                       , _verTMAcceptance       = StTmDisabled
                                       , _verTMStart            = StTmDisabled
                                       , _verTMProgress         = V.empty
                                       , _verTMComplete         = StTmDisabled
                                       , _verStatus = VerifStatNominal
                                       }




setReleaseStage :: ReleaseStage -> Verification -> Verification
setReleaseStage StRFail verif =
    verif
        &  verRelease
        .~ StRFail
        &  setAllGroundStages StGDisabled
        &  setAllTMStages StTmDisabled
setReleaseStage status verif =
    setAllExpectedToPending (verif & verRelease .~ status)

setGroundReceptionStage :: GroundStage -> Verification -> Verification
setGroundReceptionStage StGFail verif =
    verif
        &  verGroundReception
        .~ StGFail
        &  verGroundTransmission
        .~ StGDisabled
        &  verGroundOBR
        .~ StGDisabled
        &  setAllTMStages StTmDisabled
setGroundReceptionStage StGTimeout verif = 
    if (verif ^. verGroundReception == StGPending) || (verif ^. verGroundReception == StGExpected)
        then verif & verGroundReception .~ StGTimeout
        else verif 
setGroundReceptionStage status verif = verif & verGroundReception .~ status


setGroundTransmissionStage :: GroundStage -> Verification -> Verification
setGroundTransmissionStage StGFail verif =
    verif
        &  verGroundTransmission
        .~ StGFail
        &  verGroundOBR
        .~ StGDisabled
        &  setAllTMStages StTmDisabled
setGroundTransmissionStage StGSuccess verif =
    let newVerif = if verif ^. verGroundReception /= StGSuccess
            then verif & verGroundReception .~ StGAssumed
            else verif
    in  newVerif & verGroundTransmission .~ StGSuccess
setGroundTransmissionStage StGTimeout verif = 
    if (verif ^. verGroundTransmission == StGPending) || (verif ^. verGroundTransmission == StGExpected)
        then verif & verGroundTransmission .~ StGTimeout
        else verif
setGroundTransmissionStage status verif =
    verif & verGroundTransmission .~ status

setGroundOBRStage :: GroundStage -> Verification -> Verification
setGroundOBRStage StGFail verif =
    verif & verGroundOBR .~ StGFail & setAllTMStages StTmDisabled
setGroundOBRStage StGSuccess verif =
    let verif1 = if verif ^. verGroundReception /= StGSuccess
            then verif & verGroundReception .~ StGAssumed
            else verif
        verif2 = if verif1 ^. verGroundTransmission /= StGSuccess
            then verif1 & verGroundTransmission .~ StGAssumed
            else verif1
    in  verif2 & verGroundOBR .~ StGSuccess
setGroundOBRStage StGTimeout verif = 
    if (verif ^. verGroundOBR == StGPending) || (verif ^. verGroundOBR == StGExpected)
        then verif & verGroundOBR .~ StGTimeout
        else verif
setGroundOBRStage status verif = verif & verGroundOBR .~ status

setGroundGTStages :: GroundStage -> Verification -> Verification
setGroundGTStages StGFail verif =
    verif
        &  verGroundReception
        .~ StGFail
        &  verGroundTransmission
        .~ StGFail
        &  verGroundOBR
        .~ StGDisabled
        &  setAllTMStages StTmDisabled
setGroundGTStages StGTimeout verif = 
    let newVerif = if (verif ^. verGroundReception == StGPending) || (verif ^. verGroundReception == StGExpected)
                        then verif & verGroundReception .~ StGTimeout
                        else verif 
    in 
    if (newVerif ^. verGroundTransmission == StGPending) || (newVerif ^. verGroundTransmission == StGExpected)
        then newVerif & verGroundTransmission .~ StGTimeout
        else newVerif
setGroundGTStages status verif =
    verif & verGroundReception .~ status & verGroundTransmission .~ status


setTMAcceptStage :: TMStage -> Verification -> Verification
setTMAcceptStage StTmFail verif =
    verif
        &  verTMAcceptance
        .~ StTmFail
        &  verTMStart
        .~ StTmDisabled
        &  verTMComplete
        .~ StTmDisabled
        &  verTMProgress
        .  traversed
        .~ StTmDisabled
setTMAcceptStage StTmSuccess verif =
    let obr      = newVerif ^. verGroundOBR
        newVerif = verif & verTMAcceptance .~ StTmSuccess
    in  if (obr == StGExpected) || (obr == StGPending)
            then newVerif & verGroundOBR .~ StGAssumed
            else newVerif
setTMAcceptStage StTmAssumed verif =
    let obr      = newVerif ^. verGroundOBR
        newVerif = verif & verTMAcceptance .~ StTmAssumed
    in  if (obr == StGExpected) || (obr == StGPending)
            then newVerif & verGroundOBR .~ StGAssumed
            else newVerif
setTMAcceptStage StTmTimeout verif = 
    if (verif ^. verTMAcceptance == StTmPending) || (verif ^. verTMAcceptance == StTmExpected)
        then verif & verTMAcceptance .~ StTmTimeout
        else verif
setTMAcceptStage status verif = verif & verTMAcceptance .~ status


setTMStartStage :: TMStage -> Verification -> Verification
setTMStartStage StTmFail verif =
    verif
        &  verTMStart
        .~ StTmFail
        &  verTMComplete
        .~ StTmDisabled
        &  verTMProgress
        .  traversed
        .~ StTmDisabled
setTMStartStage StTmSuccess verif =
    setAcceptPending (verif & verTMStart .~ StTmSuccess)
setTMStartStage StTmAssumed verif =
    setAcceptPending (verif & verTMStart .~ StTmAssumed)
setTMStartStage StTmTimeout verif = 
    if (verif ^. verTMStart == StTmPending) || (verif ^. verTMStart == StTmExpected)
        then verif & verTMStart .~ StTmTimeout
        else verif
setTMStartStage status verif = verif & verTMStart .~ status

setAcceptPending :: Verification -> Verification
setAcceptPending newVerif =
    let acc = newVerif ^. verTMAcceptance
    in  if (acc == StTmExpected) || (acc == StTmPending)
            then setTMAcceptStage StTmAssumed newVerif
            else newVerif


setTMProgressStage :: Int -> TMStage -> Verification -> Verification
setTMProgressStage i status verif =
    let prog = (verif ^. verTMProgress)
    in  if i >= 0 && i < V.length prog
            then verif & verTMProgress .~ setProg prog status
            else verif
  where
    setProg prog StTmFail    = V.imap setFail prog
    setProg prog StTmSuccess = V.imap (setp StTmSuccess) prog
    setProg prog StTmAssumed = V.imap (setp StTmAssumed) prog
    setProg prog StTmTimeout = V.imap (setp StTmTimeout) prog
    setProg prog st          = prog V.// [(i, st)]


    setFail ix v = if
        | i < ix    -> StTmDisabled
        | i == ix   -> StTmFail
        | otherwise -> v

    setp st ix v = if
        | i == ix -> st
        | i > ix -> if (v == StTmExpected) || (v == StTmPending)
            then StTmAssumed
            else v
        | otherwise -> v


setProgressAssumed :: Verification -> Verification
setProgressAssumed verif = verif & verTMProgress . traversed %~ setp
  where
    setp x = if x == StTmExpected || x == StTmPending then StTmAssumed else x



setTMCompleteStage :: TMStage -> Verification -> Verification
setTMCompleteStage StTmFail verif =
    verif & verTMComplete .~ StTmFail & setStartPending & setProgressAssumed
setTMCompleteStage StTmSuccess verif =
    verif & verTMComplete .~ StTmSuccess & setStartPending & setProgressAssumed
setTMCompleteStage StTmTimeout verif = 
    if (verif ^. verTMComplete == StTmPending) || (verif ^. verTMComplete == StTmExpected)
        then verif & verTMComplete .~ StTmTimeout
        else verif
setTMCompleteStage status verif = verif & verTMComplete .~ status


setStartPending :: Verification -> Verification
setStartPending newVerif =
    let acc = newVerif ^. verTMStart
    in  if (acc == StTmExpected) || (acc == StTmPending)
            then setTMStartStage StTmAssumed newVerif
            else newVerif



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

setAllExpectedToPending :: Verification -> Verification
setAllExpectedToPending verif =
    gr verGroundReception verif
        &  gr verGroundTransmission
        &  gr verGroundOBR
        &  tm verTMAcceptance
        &  tm verTMStart
        &  tm verTMComplete
        &  verTMProgress
        .  traversed
        %~ \x -> if x == StTmExpected then StTmPending else x
  where
    gr :: Lens' Verification GroundStage -> Verification -> Verification
    gr l x = if x ^. l == StGExpected then x & l .~ StGPending else x

    tm :: Lens' Verification TMStage -> Verification -> Verification
    tm l x = if x ^. l == StTmExpected then x & l .~ StTmPending else x

isFailed :: Verification -> Bool
isFailed Verification {..} =
    (_verRelease == StRFail)
        || (_verGroundReception == StGFail)
        || (_verGroundTransmission == StGFail)
        || (_verGroundOBR == StGFail)
        || (_verTMAcceptance == StTmFail)
        || (_verTMStart == StTmFail)
        || (_verTMComplete == StTmFail)
        || V.any (== StTmFail) _verTMProgress


isTMExpected :: Verification -> Bool
isTMExpected verif =
    isTMAExpected verif 
        || isTMSExpected verif
        || isTMCExpected verif
  -- we don't check for the progess, because for progess we need at 
  -- least a completion anyway


isTMAExpected :: Verification -> Bool 
isTMAExpected Verification {..} =
    (_verTMAcceptance == StTmExpected) || (_verTMAcceptance == StTmPending)

isTMSExpected :: Verification -> Bool 
isTMSExpected Verification {..} =
    (_verTMStart == StTmExpected) || (_verTMStart == StTmPending)

isTMCExpected :: Verification -> Bool 
isTMCExpected Verification {..} =
    (_verTMComplete == StTmExpected) || (_verTMComplete == StTmPending)


isGroundSuccess :: Verification -> Bool
isGroundSuccess Verification {..} =
    (_verGroundOBR == StGSuccess)
    || (_verGroundOBR == StGDisabled) && (_verGroundTransmission == StGSuccess)

isGroundTimeout :: Verification -> Bool 
isGroundTimeout Verification {..} = 
    (_verGroundOBR == StGTimeout)
    || (_verGroundOBR == StGDisabled) && (_verGroundTransmission == StGTimeout)

isGroundDisabled :: Verification -> Bool 
isGroundDisabled Verification {..} = 
    (_verGroundReception == StGDisabled) 
    && (_verGroundTransmission == StGDisabled)
    && (_verGroundOBR == StGDisabled)

isGroundExpected :: Verification -> Bool 
isGroundExpected Verification {..} =
    ((_verGroundReception == StGExpected) || (_verGroundReception == StGPending))
    || ((_verGroundTransmission == StGExpected) || (_verGroundTransmission == StGPending))
    || ((_verGroundOBR == StGExpected) || (_verGroundOBR == StGPending))

isGroundFail :: Verification -> Bool 
isGroundFail Verification {..} =
    (_verGroundReception == StGFail)
    || (_verGroundTransmission == StGFail)
    || (_verGroundOBR == StGFail)

isOBAExpected :: Verification -> Bool 
isOBAExpected Verification {..} = 
    (_verGroundOBR == StGExpected) || (_verGroundOBR == StGPending)

isSuccess :: Verification -> Bool
isSuccess verif@Verification {..} =
    (_verTMComplete == StTmSuccess)
    || (not (isTMExpected verif) && isGroundSuccess verif)
    || (isGroundDisabled verif && (_verRelease == StRSuccess))


isTimeout :: Verification -> Bool 
isTimeout verif@Verification {..} = 
    (_verTMComplete == StTmTimeout)
    || not (isTMExpected verif) && isGroundTimeout verif 


isFinished :: Verification -> Bool 
isFinished verif = 
    isFailed verif || isSuccess verif || isTimeout verif 