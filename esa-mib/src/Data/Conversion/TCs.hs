{-# LANGUAGE  TemplateHaskell #-}
module Data.Conversion.TCs
    ( convertTCDef
    , convertTCParams
    , convertTCParamLocDef
    , convertCCF
    , convertCPC
    , convertPRF
    , convertRange
    ) where

import           RIO
import qualified RIO.Vector                    as V
import qualified RIO.Text                      as T
import qualified RIO.HashMap                   as HM
import           RIO.List                       ( sortOn
                                                , sort
                                                )
import           Data.HashTable.ST.Basic        ( IHashTable )
import qualified Data.HashTable.ST.Basic       as HT

import qualified Data.Text.Short               as ST
import           RIO.List                       ( intersperse )

import           Data.Text.Short                ( ShortText )
import           Control.Monad.Except
import           Refined.Unsafe                 ( reallyUnsafeRefine )


import           Data.PUS.Value
import           Data.PUS.EncTime

import           General.Types
import           General.Time                   ( mcsTimeToOBT
                                                , CorrelationCoefficients
                                                , Epoch
                                                )
import           General.PUSTypes
import           General.APID

import           Data.TC.TCParameterDef
import           Data.TC.RangeSet               ( RangeSet(..)
                                                , RangeValue(..)
                                                )
import           Data.TC.Calibration
import           Data.TC.TCDef

import           Data.TM.Value

import           Data.MIB.Types
import           Data.MIB.CPC
import           Data.MIB.PRV
import           Data.MIB.PRF
import           Data.MIB.CDF
import           Data.MIB.CCF
import           Data.MIB.CVS
import           Data.MIB.CVP
import           Data.MIB.SCO
import           Data.MIB.TCD

import           Data.Conversion.Types

import           Data.Multimap                  ( Multimap )
import qualified Data.Multimap                 as M



convertTCDef
    :: Epoch
    -> CorrelationCoefficients
    -> ShortText
    -> HashMap ShortText PRFentry
    -> Multimap ShortText PRVentry
    -> HashMap ShortText TCNumericCalibration
    -> HashMap ShortText TCTextCalibration
    -> Multimap ShortText CDFentry
    -> Vector CPCentry
    -> HashMap Int CVSentry
    -> Multimap ShortText CVPentry
    -> HashMap Word32 TCDentry
    -> HashMap ShortText SCOentry
    -> Vector CCFentry
    -> ( [Text]
       , IHashTable ShortText TCDef
       )
convertTCDef epoch coeff defaultConnName prfMap prvs numCalibs textCalibs cdfs cpcs cvsMap cvpMap tcdMap scoMap vec
    = let (errs, tcs) =
              partitionEithers
                  . V.toList
                  . V.map
                        (convertCCF epoch
                                    coeff
                                    defaultConnName
                                    prfMap
                                    prvs
                                    numCalibs
                                    textCalibs
                                    cdfs
                                    cpcs
                                    cvsMap
                                    cvpMap
                                    tcdMap
                                    scoMap
                        )
                  $ vec
          conv x = (_tcDefName x, x)
          hm = HT.fromList $ map conv tcs
      in  (errs, hm)



convertCCF
    :: Epoch
    -> CorrelationCoefficients
    -> ShortText
    -> HashMap ShortText PRFentry
    -> Multimap ShortText PRVentry
    -> HashMap ShortText TCNumericCalibration
    -> HashMap ShortText TCTextCalibration
    -> Multimap ShortText CDFentry
    -> Vector CPCentry
    -> HashMap Int CVSentry
    -> Multimap ShortText CVPentry
    -> HashMap Word32 TCDentry
    -> HashMap ShortText SCOentry
    -> CCFentry
    -> Either Text TCDef
convertCCF epoch coeff defaultConnName prfMap prvs numCalibs textCalibs cdfs cpcs cvsMap cvpMap tcdMap scoMap CCFentry {..}
    = let locs   = sortOn (_cdfBit) $ M.lookup _ccfName cdfs
          cpcMap = getCPCMap cpcs
          conv cdf = if _cdfElemType cdf == 'A'
              then createFixedArea cdf
              else case HM.lookup (_cdfPName cdf) cpcMap of
                  Just cpc -> convertTCParamLocDef epoch
                                                   coeff
                                                   prfMap
                                                   prvs
                                                   numCalibs
                                                   textCalibs
                                                   cpc
                                                   cdf
                  Nothing ->
                      Left
                          $  "TC Location specifies parameter '"
                          <> ST.toText (_cdfPName cdf)
                          <> "', but this is not found in CPC table"
          (errs'    , plocs ) = partitionEithers . map conv $ locs
          (stageErrs, stages) = convertVerif _ccfName cvsMap cvpMap
          errs                = errs' ++ stageErrs
      in  if not (null errs)
              then
                  Left
                  $  "Error converting TC from MIB: "
                  <> T.intercalate "\n" errs
              else
                  let
                      (connName, connFlag) = getConnectionName
                          defaultConnName
                          tcdMap
                          scoMap
                          _ccfApid
                  in
                      Right $ TCDef
                          { _tcDefName           = _ccfName
                          , _tcDefDescr          = _ccfDescr
                          , _tcDefDescr2         = _ccfDescr2
                          , _tcDefCType          = determineCType _ccfCType
                          , _tcDefCritical = _ccfCritical == CharDefaultTo 'Y'
                          , _tcDefApid = APID . fromIntegral <$> _ccfApid
                          , _tcDefType = PUSType . fromIntegral <$> _ccfType
                          , _tcDefSubType        = PUSSubType
                                                   .   fromIntegral
                                                   <$> _ccfSType
                          , _tcDefExec           = _ccfExec == CharDefaultTo 'Y'
                          , _tcDefILScope        = determineILScope _ccfIlScope
                          , _tcDefILStage        = determineILStage _ccfIlStage
                          , _tcDefSubSys         = _ccfSubSys
                          , _tcDefMapID = mkMAPID . fromIntegral <$> _ccfMapID
                          , _tcDefParamSet       = ParamSet -- TODO
                          , _tcDefAckFlags       = maybe 0 fromIntegral _ccfAck
                          , _tcDefSubSched       = _ccfSubschedID
                          , _tcDefVerifStages    = stages
                          , _tcDefConnection     = connName
                          , _tcDefConnectionFlag = connFlag
                          , _tcDefParams         = V.fromList plocs
                          }

  where
    determineCType "R" = TCControlSegment
    determineCType "F" = TCControlFrame
    determineCType "S" = TCNoCRC
    determineCType "T" = TCSleThrowEvent
    determineCType "N" = TCNisThrowEvent
    determineCType _   = TCNormal

    determineILScope (CharDefaultTo 'G') = ILGlobal
    determineILScope (CharDefaultTo 'L') = ILLocal
    determineILScope (CharDefaultTo 'S') = ILSubSystem
    determineILScope (CharDefaultTo 'B') = ILGlobalSubsystem
    determineILScope _                   = ILNone

    determineILStage (CharDefaultTo 'R') = ILRelease
    determineILStage (CharDefaultTo 'U') = ILUplink
    determineILStage (CharDefaultTo 'O') = ILOnboardReception
    determineILStage (CharDefaultTo 'A') = ILAcceptance
    determineILStage (CharDefaultTo 'C') = ILCompletion
    determineILStage _                   = ILCompletion


createFixedArea :: CDFentry -> Either Text TCParameterLocDef
createFixedArea CDFentry {..} =
    let param val = TCParameterDef { _tcpName         = "FIXED"
                                   , _tcpDescr        = "Fixed area"
                                   , _tcpPTC          = PTC 3
                                   , _tcpPFC          = PFC (_cdfElemLen - 4)
                                   , _tcpDefaultValue = TCParamRaw val
                                   , _tcpRadix        = Hex
                                   , _tcpUnit         = ""
                                   , _tcpProcType     = TCParamNormal
                                   , _tcpCalib        = Nothing
                                   , _tcpRange        = Nothing
                                   , _tcpCorrelate    = CorrelationNo
                                   , _tcpObtID        = 0
                                   }
    in
        case parseShortTextToWord64 NumUInteger Hex _cdfValue of
            Left err ->
                Left
                    $  "CDF fixed area: cannot parse value: "
                    <> ST.toText _cdfValue
                    <> ": "
                    <> err
            Right def -> case determineRawValue def of
                Left  err -> Left err
                Right val -> Right $ TCParameterLocDef
                    { _tcplElemType     = determineElemType _cdfElemType
                    , _tcplDescr        = _cdfDescr
                    , _tcplLen          = BitSize _cdfElemLen
                    , _tcplBit          = BitOffset _cdfBit
                    , _tcplGroupSize = fromIntegral (getDefaultInt _cdfGrpSize)
                    , _tcplElemFlag     = determineElemFlag _cdfInter
                    , _tcplDefaultValue = TCParamRaw val
                    , _tcplTMParam      = _cdfTmID
                    , _tcplParam        = param val
                    }
  where
    determineRawValue val
        | _cdfElemLen >= 1 && _cdfElemLen <= 7
        = Right $ ValUInt8X
            (B8 (reallyUnsafeRefine (fromIntegral _cdfElemLen)))
            (fromIntegral val)
        | _cdfElemLen == 8
        = Right $ ValUInt8 (fromIntegral val)
        | _cdfElemLen >= 9 && _cdfElemLen <= 15
        = Right $ ValUInt16X
            (B16 (reallyUnsafeRefine (fromIntegral _cdfElemLen)))
            (fromIntegral val)
        | _cdfElemLen == 16
        = Right $ ValUInt16 BiE (fromIntegral val)
        | _cdfElemLen == 24
        = Right $ ValUInt24 BiE (fromIntegral val)
        | _cdfElemLen >= 17 && _cdfElemLen <= 31
        = Right $ ValUInt32X
            (B32 (reallyUnsafeRefine (fromIntegral _cdfElemLen)))
            (fromIntegral val)
        | _cdfElemLen == 32
        = Right $ ValUInt32 BiE (fromIntegral val)
        | _cdfElemLen == 64
        = Right $ ValUInt64 BiE val
        | otherwise
        = Left
            $  "Illegal fixed are specification CDF_ELLEN="
            <> textDisplay _cdfElemLen


convertTCParamLocDef
    :: Epoch
    -> CorrelationCoefficients
    -> HashMap ShortText PRFentry
    -> Multimap ShortText PRVentry
    -> HashMap ShortText TCNumericCalibration
    -> HashMap ShortText TCTextCalibration
    -> CPCentry
    -> CDFentry
    -> Either Text TCParameterLocDef
convertTCParamLocDef epoch coeff prfMap prvs numCalibs textCalibs cpc@CPCentry {..} CDFentry {..}
    = let (ptc, pfc, _) = getPtcPfc _cpcCateg _cpcPTC _cpcPFC
          def'          = determineDefaultValue
              epoch
              coeff
              ptc
              pfc
              (getDefaultChar _cpcInter)
              (getDefaultChar _cpcDispFmt)
              (charToRadix (getDefaultChar _cpcRadix))
              _cdfValue
              cpc
          def    = either (const TCParamNothing) id def'
          param' = convertCPC epoch coeff prfMap prvs numCalibs textCalibs cpc
      in  case param' of
              Left  err   -> Left err
              Right param -> Right $ TCParameterLocDef
                  { _tcplElemType     = determineElemType _cdfElemType
                  , _tcplDescr        = _cdfDescr
                  , _tcplLen          = BitSize _cdfElemLen
                  , _tcplBit          = BitOffset _cdfBit
                  , _tcplGroupSize    = fromIntegral (getDefaultInt _cdfGrpSize)
                  , _tcplElemFlag     = determineElemFlag _cdfInter
                  , _tcplDefaultValue = def
                  , _tcplTMParam      = _cdfTmID
                  , _tcplParam        = param
                  }

determineElemType :: Char -> ElemType
determineElemType 'A' = ElemFixedArea
determineElemType 'F' = ElemFixed
determineElemType 'E' = ElemEditable
determineElemType _   = ElemEditable

determineElemFlag :: CharDefaultTo a -> ElemFlag
determineElemFlag (CharDefaultTo 'R') = ElemRaw
determineElemFlag (CharDefaultTo 'E') = ElemEng
determineElemFlag (CharDefaultTo 'D') = ElemCPC
determineElemFlag (CharDefaultTo 'T') = ElemTM
determineElemFlag _                   = ElemRaw


convertTCParams
    :: Epoch
    -> CorrelationCoefficients
    -> HashMap ShortText PRFentry
    -> Multimap ShortText PRVentry
    -> HashMap ShortText TCNumericCalibration
    -> HashMap ShortText TCTextCalibration
    -> Vector CPCentry
    -> ([Text], HashMap ShortText TCParameterDef)
convertTCParams epoch coeff prfs prvs numCalibs textCalibs vec =
    let (errs, params) =
            partitionEithers
                . V.toList
                . V.map (convertCPC epoch coeff prfs prvs numCalibs textCalibs)
                $ vec
        hm = foldl' (\h x -> HM.insert (_tcpName x) x h) HM.empty params
    in  (errs, hm)




getPtcPfc :: CharDefaultTo a -> Int -> Int -> (PTC, PFC, TCParamType)
getPtcPfc categ ptc pfc = case getDefaultChar categ of
    'A' -> (PTC 7, PFC 0, TCParamCmdID)
    'P' -> (PTC 3, PFC pfc, TCParamParamID)
    _   -> (PTC ptc, PFC pfc, TCParamNormal)

convertCPC
    :: Epoch
    -> CorrelationCoefficients
    -> HashMap ShortText PRFentry
    -> Multimap ShortText PRVentry
    -> HashMap ShortText TCNumericCalibration
    -> HashMap ShortText TCTextCalibration
    -> CPCentry
    -> Either Text TCParameterDef
convertCPC epoch coeff prfMap prvVec numCalibs textCalibs cpc@CPCentry {..} =
    let rangeSet = if ST.null _cpcPrfRef
            then Right Nothing
            else case HM.lookup _cpcPrfRef prfMap of
                Nothing ->
                    Left
                        $  "PRF entry for parameter '"
                        <> ST.toText _cpcName
                        <> "' not found, PRF_REF="
                        <> ST.toText _cpcPrfRef
                Just prf -> Just <$> convertPRF prf prvVec
        corr = case getDefaultChar _cpcCorr of
            'N' -> CorrelationNo
            _   -> CorrelationYes
        (ptc, pfc, t) = getPtcPfc _cpcCateg _cpcPTC _cpcPFC
    in  runExcept $ do
            s   <- liftEither $ rangeSet
            def <- liftEither $ determineDefaultValue
                epoch
                coeff
                ptc
                pfc
                (getDefaultChar _cpcInter)
                (getDefaultChar _cpcDispFmt)
                (charToRadix (getDefaultChar _cpcRadix))
                _cpcDefVal
                cpc
            return $ TCParameterDef
                { _tcpName         = _cpcName
                , _tcpDescr        = _cpcDescr
                , _tcpPTC          = PTC _cpcPTC
                , _tcpPFC          = PFC _cpcPFC
                , _tcpDefaultValue = def
                , _tcpRadix        = charToRadix (getDefaultChar _cpcRadix)
                , _tcpUnit         = _cpcUnit
                , _tcpProcType     = t
                , _tcpCalib        = determineCalib numCalibs textCalibs cpc
                , _tcpRange        = s
                , _tcpCorrelate    = corr
                , _tcpObtID        = getDefaultInt _cpcObtID
                }


determineDefaultValue
    :: Epoch
    -> CorrelationCoefficients
    -> PTC
    -> PFC
    -> Char
    -> Char
    -> Radix
    -> ShortText
    -> CPCentry
    -> Either Text TCParamDefaultValue
determineDefaultValue epoch coeff ptc pfc inter dispFormat radix val cpc
    | ST.null val = Right TCParamNothing
    | otherwise = case getDefaultChar (_cpcCateg cpc) of
        'A' -> Right $ TCCmdID val
        'P' -> Right $ TCParamID val
        _   -> case inter of
            'E' -> case getVal dispFormat radix val of
                Left err ->
                    Left
                        $  "Could not get default engineering value from '"
                        <> ST.toText val
                        <> "': "
                        <> err
                Right v -> Right $ TCParamEng v
            _ ->
                TCParamRaw
                    <$> determineRawDefaultValue epoch coeff ptc pfc val cpc


determineRawDefaultValue
    :: Epoch
    -> CorrelationCoefficients
    -> PTC
    -> PFC
    -> ShortText
    -> CPCentry
    -> Either Text Value
determineRawDefaultValue epoch coeff ptc pfc val cpc =
    case parseShortTextToValueSimple ptc pfc val of
        Left err ->
            Left
                $  "could not determine raw default value: PTC="
                <> textDisplay ptc
                <> " PFC="
                <> textDisplay pfc
                <> ": "
                <> err
                <> "\nCPC Entry: "
                <> T.pack (show cpc)
        Right v ->
            let rawVal = initialValue BiE ptc pfc
            in
                Right $ case v of
                    TMValInt    x -> Data.PUS.Value.setInt rawVal x
                    TMValUInt   x -> Data.PUS.Value.setInt rawVal x
                    TMValDouble x -> setDouble rawVal x
                    TMValTime   x -> ValCUCTime
                        $ sunTimeToCUCTime epoch (mcsTimeToOBT x coeff)
                    TMValString x -> setString rawVal (ST.toText x)
                    TMValOctet  x -> setOctet rawVal x
                    TMValNothing  -> ValUndefined



determineCalib
    :: HashMap ShortText TCNumericCalibration
    -> HashMap ShortText TCTextCalibration
    -> CPCentry
    -> Maybe TCCalibration
determineCalib numCalibs _textCalibs CPCentry { _cpcCateg = CharDefaultTo 'C', _cpcCcaRef = ref }
    = TCNumCalib <$> HM.lookup ref numCalibs
determineCalib _numCalibs textCalibs CPCentry { _cpcCateg = CharDefaultTo 'T', _cpcPafRef = ref }
    = TCTextCalib <$> HM.lookup ref textCalibs
determineCalib _ _ _ = Nothing


convertPRF :: PRFentry -> Multimap ShortText PRVentry -> Either Text RangeSet
convertPRF prf@PRFentry {..} m =
    let (errs, prvs) =
            partitionEithers . map (convertRange prf) . M.lookup _prfNumbr $ m
    in  if not (RIO.null errs)
            then
                Left
                $  "Error converting PRVs: "
                <> (mconcat . intersperse "\n") errs
            else Right $ RangeSet { _rsIdent  = _prfNumbr
                                  , _rsDescr  = _prfDescr
                                  , _rsInter  = sel
                                  , _rsValues = V.fromList prvs
                                  }
  where
    sel = case getDefaultChar _prfInter of
        'R' -> InterRaw
        'E' -> InterEng
        _   -> InterRaw



convertRange :: PRFentry -> PRVentry -> Either Text RangeValue
convertRange PRFentry {..} PRVentry {..} = if ST.null _prvMaxVal
    then case getVal (getDefaultChar _prfDispFormat) radix _prvMinVal of
        Left  err -> Left $ "Could not convert range discrete value: " <> err
        Right v   -> Right (RangeDiscrete v)
    else
        let res = runExcept $ do
                v1 <- liftEither $ getVal (getDefaultChar _prfDispFormat)
                                          radix
                                          _prvMinVal
                v2 <- liftEither $ getVal (getDefaultChar _prfDispFormat)
                                          radix
                                          _prvMaxVal
                return $ RangeMinMax v1 v2
        in  case res of
                Left err ->
                    Left $ "Could not convert range min or max value: " <> err
                Right v -> Right v
    where radix = charToRadix . getDefaultChar $ _prfRadix




convertVerif
    :: ShortText
    -> HashMap Int CVSentry
    -> Multimap ShortText CVPentry
    -> ([Text], Vector VerificationDef)
convertVerif cmdName cvsMap cvpMap =
    let verifs = M.lookup cmdName cvpMap
        stage stageID = case HM.lookup stageID cvsMap of
            Nothing ->
                Left
                    $  "Verification stage with ID "
                    <> textDisplay stageID
                    <> " not found in CVS map"
            Just st -> Right st
        (errs, cvss) = partitionEithers $ map (stage . _cvpCvsID) verifs
        convType 'A' = VerStageA
        convType 'S' = VerStageS
        convType '0' = VerStageP 0
        convType '1' = VerStageP 1
        convType '2' = VerStageP 2
        convType '3' = VerStageP 3
        convType '4' = VerStageP 4
        convType '5' = VerStageP 5
        convType '6' = VerStageP 6
        convType '7' = VerStageP 7
        convType '8' = VerStageP 8
        convType '9' = VerStageP 9
        convType 'C' = VerStageC
        convType _   = VerStageNone

        stages = map (convType . _cvsType) cvss
    in  (errs, V.fromList (sort stages))



getConnectionName
    :: ShortText
    -> HashMap Word32 TCDentry
    -> HashMap ShortText SCOentry
    -> Maybe Int
    -> (ShortText, CommandType)
getConnectionName defaultLinkName _tcdMap _scoMap Nothing =
    (defaultLinkName, SCOE)
getConnectionName defaultLinkName tcdMap scoMap (Just apid) =
    let look = do
            tcd <- HM.lookup (fromIntegral apid) tcdMap
            sco <- HM.lookup (_tcdLink tcd) scoMap
            let flag = case _tcdFlag tcd of
                    'D' -> Space ProtLevelPacket
                    'S' -> SCOE
                    _   -> SCOE
            return (_scoSCOE sco, flag)
    in  fromMaybe (defaultLinkName, SCOE) look
