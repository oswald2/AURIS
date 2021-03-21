module Data.PUS.TCGeneration
    ( generateTC
    ) where

import           RIO                     hiding ( (.~)
                                                , (^.)
                                                )
import qualified RIO.Vector                    as V
import           RIO.List                       ( headMaybe )

import           Data.Text.Short                ( ShortText )
import           Control.Lens

import           Data.PUS.TCRequest
import           Data.PUS.Verification
import           Data.PUS.Config

import           Data.TC.TCDef

import           General.PUSTypes


generateTC :: Config -> ShortText -> TransmissionMode -> TCDef -> TCRequest
generateTC cfg source transMode tcDef = TCRequest
    { _tcReqRequestID     = mkRqstID 0
    , _tcReqName          = tcDef ^. tcDefName
    , _tcReqDescription   = tcDef ^. tcDefDescr
    , _tcReqSource        = source
    , _tcReqReleaseTime   = Nothing
    , _tcReqVerifications = genVerification transMode (tcDef ^. tcDefVerifStages)
    , _tcReqSCID          = cfgSCID cfg
    , _tcReqVCID          = fromMaybe 0 $ headMaybe (cfgVCIDs cfg)
    , _tcReqPayload       = genPayload tcDef
    }


genVerification :: TransmissionMode -> Vector VerificationDef -> Verification
genVerification transMode ls = 
    let verif = case transMode of 
                    AD -> defaultVerificationAD
                    BD -> defaultVerificationBD
    in
    foldl' genStage emptyVerification ls
  where
    genStage verif VerStageNone = verif
    genStage verif VerStageA    = verif & verTMAcceptance .~ StTmExpected
    genStage verif VerStageS    = verif & verTMStart .~ StTmExpected
    genStage verif VerStageC    = verif & verTMComplete .~ StTmExpected
    genStage verif (VerStageP x) =
        let xnew     = if x >= 9 then 9 else if x < 0 then 0 else x
            newVerif = if V.null (verif ^. verTMProgress)
                then verif & verTMProgress .~ V.replicate 10 StTmDisabled
                else verif
        in  newVerif & verTMProgress . ix xnew .~ StTmExpected

genPayload :: TCDef -> TCRequestBody
genPayload tcDef = undefined
