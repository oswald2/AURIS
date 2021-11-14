module Data.PUS.TCGeneration
    ( getTC
    , generateTC
    ) where

import           RIO                     hiding ( (.~)
                                                , (^.)
                                                )
import qualified RIO.Vector                    as V
import qualified RIO.HashMap                   as HM
--import qualified RIO.ByteString                as B
import qualified RIO.Text                      as T
import           RIO.List                       ( headMaybe )

import           Data.Text.Short                ( ShortText )
import qualified Data.Text.Short               as ST
import           Control.Lens            hiding ( Empty
                                                , List
                                                )

import           Data.PUS.TCRequest
import           Data.PUS.TCPacket
import           Data.PUS.Verification
import           Data.PUS.Config
import           Data.PUS.Parameter
import           Data.PUS.Value
import           Data.PUS.CalibrationTypes
import           Data.PUS.TCCnc
import           Data.PUS.PUSState

import           Data.TC.TCDef
import           Data.TC.TCParameterDef
import           Data.TC.Calibration
import           Data.TC.ValueConversion

import           Data.TM.Value

import           General.PUSTypes
import           General.Time
import           General.APID

import           Control.PUS.Classes

import           Protocol.ProtocolInterfaces



getTC
    :: (MonadIO m, MonadReader env m, HasGlobalState env)
    => ShortText
    -> TransmissionMode
    -> TCDef
    -> m TCRequest
getTC source transMode tcDef = do
    env            <- ask
    (epoch, coeff) <- atomically $ do
        ep <- _pusStEpoch <$> readTVar (env ^. appStateG)
        c  <- readTVar (env ^. corrStateG)
        return (ep, c)
    let cfg    = env ^. getConfig
        cfgMap = getInterfaceMap cfg -- TODO don't generate map on every entry...
    return $ generateTC cfg source epoch coeff cfgMap transMode tcDef

generateTC
    :: Config
    -> ShortText
    -> Epoch
    -> CorrelationCoefficients
    -> HashMap ShortText ProtocolInterface
    -> TransmissionMode
    -> TCDef
    -> TCRequest
generateTC cfg source epoch coeff connMap transMode tcDef = TCRequest
    { _tcReqRequestID     = mkRqstID 0
    , _tcReqName          = tcDef ^. tcDefName
    , _tcReqDescription   = tcDef ^. tcDefDescr
    , _tcReqSource        = source
    , _tcReqReleaseTime   = Nothing
    , _tcReqVerifications = genVerification transMode
                                            (tcDef ^. tcDefVerifStages)
    , _tcReqSCID          = cfgSCID cfg
    , _tcReqVCID          = fromMaybe 0 $ headMaybe (cfgVCIDs cfg)
    , _tcReqPayload       = genPayload epoch coeff connMap transMode tcDef
    }


genVerification :: TransmissionMode -> Vector VerificationDef -> Verification
genVerification transMode ls =
    let verif = case transMode of
            AD -> defaultVerificationAD
            BD -> defaultVerificationBD
    in  foldl' genStage verif ls
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



genPayload
    :: Epoch
    -> CorrelationCoefficients
    -> HashMap ShortText ProtocolInterface
    -> TransmissionMode
    -> TCDef
    -> TCRequestBody
genPayload epoch coeff connMap transMode tcDef =
    case (tcDef ^. tcDefApid, tcDef ^. tcDefType, tcDef ^. tcDefSubType) of
        (Just apid, Just t, Just st) -> TCCommand
            { _tcReqMAPID     = fromMaybe (mkMAPID 0) (tcDef ^. tcDefMapID)
            , _tcReqTransMode = transMode
            , _tcDestination  = genDestination (tcDef ^. tcDefConnection)
            , _tcSSC          = mkSSC 0
            , _tcReqPacket    = genPacket epoch coeff apid t st tcDef
            }
        _ -> trace
            ("TC Structure not yet implemented: " <> T.pack (show tcDef))
            TCScoeCommand
                { _tcReqDestination = ScoeDestCnc (IfCnc 1)
                , _tcSSC            = mkSSC 0
                , _tcReqCommand     = TCScoe
                    { _tccAPID   = APID 0
                    , _tccParams = "Command structure not yet implemented"
                    }
                }

  where
    genDestination conn = case HM.lookup conn connMap of
        Just c@(IfNctrs _) -> DestNctrs c
        Just c@(IfEden  _) -> DestEden c (tcDef ^. tcDefConnectionFlag)
        Just c@(IfCnc   _) -> DestCnc c
        Just c@(IfSle   _) -> DestSLE c
        Nothing            -> DestNctrs (IfNctrs 1)


genPacket
    :: Epoch
    -> CorrelationCoefficients
    -> APID
    -> PUSType
    -> PUSSubType
    -> TCDef
    -> TCPacket
genPacket epoch coeff apid t st tcDef = TCPacket
    { _tcpAPID     = apid
    , _tcpType     = t
    , _tcpSubType  = st
    , _tcpSourceID = SourceID 0
    , _tcpParams   = genParameters epoch coeff (tcDef ^. tcDefParams)
    }

genParameters
    :: Epoch
    -> CorrelationCoefficients
    -> Vector TCParameterLocDef
    -> ParameterList
genParameters epoch coeff tcParamDefs =
    let
        step1 lst = span (\x -> x ^. tcplGroupSize <= 0) lst
        step2 [] = Empty
        step2 lst =
            let (ps, rest) = step1 lst
            in  List (map (genParam epoch coeff) ps) (step3 rest)
        step3 [] = Empty
        step3 (p : ps) =
            let groupSize = fromIntegral $ p ^. tcplGroupSize
            in  Group (genParam epoch coeff p) (step2 (take groupSize ps))
    in
        step2 (V.toList tcParamDefs)



genParam :: Epoch -> CorrelationCoefficients -> TCParameterLocDef -> Parameter
genParam epoch coeff def =
    let paramDef = def ^. tcplParam
    in  Parameter
            { _paramName  = ST.toText $ paramDef ^. tcpName
            , _paramValue = genParamValue epoch
                                          coeff
                                          (paramDef ^. tcpCalib)
                                          (paramDef ^. tcpPTC)
                                          (paramDef ^. tcpPFC)
                                          (paramDef ^. tcpDefaultValue)
            }


genParamValue
    :: Epoch
    -> CorrelationCoefficients
    -> Maybe TCCalibration
    -> PTC
    -> PFC
    -> TCParamDefaultValue
    -> Value
genParamValue _     _     _     _   _   TCParamNothing = trace "genParamValue: TCParamNothing" ValUndefined
genParamValue _     _     _     _   _   (TCParamRaw v) = v
genParamValue epoch coeff calib ptc pfc (TCParamEng v) = case calib of
    Just (TCNumCalib  c) -> convValue epoch coeff ptc pfc c v
    Just (TCTextCalib c) -> case v of
        TMValString t -> convValue epoch coeff ptc pfc c t
        _             -> convertValue epoch coeff ptc pfc v
    Nothing -> 
        let val = initialValue BiE ptc pfc
        in trace ("genParamValue: PTC=" <> textDisplay ptc <> " PFC=" <> textDisplay pfc <> " Value: " <> T.pack (show val)) val 
genParamValue _ _ _ _ _ (TCCmdID   v) = trace "genParamValue CmdID not yet implemented" (ValString (ST.toByteString v)) -- TODO command ID 
genParamValue _ _ _ _ _ (TCParamID v) = trace "genParamValue ParamID not yet implemented" (ValString (ST.toByteString v)) -- TODO command ID 


convValue
    :: TcCalibration a TMValueSimple c
    => Epoch
    -> CorrelationCoefficients
    -> PTC
    -> PFC
    -> a
    -> c
    -> Value
convValue epoch coeff ptc pfc c v = case tcDeCalibrate c v of
    Just v1 -> convertValue epoch coeff ptc pfc v1
    Nothing -> 
        let val = initialValue BiE ptc pfc
        in trace ("genParamValue: PTC=" <> textDisplay ptc <> " PFC=" <> textDisplay pfc <> " Value: " <> T.pack (show val)) val 
