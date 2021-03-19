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
import           RIO.List                       ( sortOn )
import           Data.HashTable.ST.Basic        ( IHashTable )
import qualified Data.HashTable.ST.Basic       as HT

import qualified Data.Text.Short               as ST
import           RIO.List                       ( intersperse )

import           Data.Text.Short                ( ShortText )
import           Control.Monad.Except


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
import           Data.Conversion.Types

import           Data.Multimap                  ( Multimap )
import qualified Data.Multimap                 as M



convertTCDef
    :: Epoch
    -> CorrelationCoefficients
    -> HashMap ShortText PRFentry
    -> Multimap ShortText PRVentry
    -> HashMap ShortText TCNumericCalibration
    -> HashMap ShortText TCTextCalibration
    -> Multimap ShortText CDFentry
    -> Vector CPCentry
    -> Vector CCFentry
    -> ([Text], IHashTable ShortText TCDef)
convertTCDef epoch coeff prfMap prvs numCalibs textCalibs cdfs cpcs vec =
    let (errs, tcs) =
            partitionEithers
                . V.toList
                . V.map (convertCCF epoch coeff prfMap prvs numCalibs textCalibs cdfs cpcs)
                $ vec
        conv x = (_tcDefName x, x)
        hm = HT.fromList $ map conv tcs
    in  (errs, hm)



convertCCF
    :: Epoch
    -> CorrelationCoefficients
    -> HashMap ShortText PRFentry
    -> Multimap ShortText PRVentry
    -> HashMap ShortText TCNumericCalibration
    -> HashMap ShortText TCTextCalibration
    -> Multimap ShortText CDFentry
    -> Vector CPCentry
    -> CCFentry
    -> Either Text TCDef
convertCCF epoch coeff prfMap prvs numCalibs textCalibs cdfs cpcs CCFentry {..}
    = let locs   = sortOn (_cdfBit) $ M.lookup _ccfName cdfs
          cpcMap = getCPCMap cpcs
          conv cdf = case HM.lookup (_cdfPName cdf) cpcMap of
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
          (errs, plocs) = partitionEithers . map conv $ locs
      in  if not (null errs)
              then
                  Left
                  $  "Error converting TC from MIB: "
                  <> T.intercalate "\n" errs
              else Right $ TCDef
                  { _tcDefName     = _ccfName
                  , _tcDefDescr    = _ccfDescr
                  , _tcDefDescr2   = _ccfDescr2
                  , _tcDefCType    = determineCType _ccfCType
                  , _tcDefCritical = _ccfCritical == CharDefaultTo 'Y'
                  , _tcDefApid     = APID . fromIntegral <$> _ccfApid
                  , _tcDefType     = PUSType . fromIntegral <$> _ccfType
                  , _tcDefSubType  = PUSSubType . fromIntegral <$> _ccfSType
                  , _tcDefExec     = _ccfExec == CharDefaultTo 'Y'
                  , _tcDefILScope  = determineILScope _ccfIlScope
                  , _tcDefILStage  = determineILStage _ccfIlStage
                  , _tcDefSubSys   = _ccfSubSys
                  , _tcDefMapID    = mkMAPID . fromIntegral <$> _ccfMapID
                  , _tcDefParamSet = ParamSet -- TODO
                  , _tcDefAckFlags = maybe 0 fromIntegral _ccfAck
                  , _tcDefSubSched = _ccfSubschedID
                  , _tcDefParams   = V.fromList plocs
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
  where
    determineElemType 'A' = ElemFixedArea
    determineElemType 'F' = ElemFixed
    determineElemType 'E' = ElemEditable
    determineElemType _   = ElemEditable

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
            return $ TCParameterDef
                { _tcpName         = _cpcName
                , _tcpDescr        = _cpcDescr
                , _tcpDefaultValue = def
                , _tcpRadix        = charToRadix (getDefaultChar _cpcRadix)
                , _tcpUnit         = _cpcUnit
                , _tcpProcType     = t
                , _tcpCalib        = determineCalib numCalibs textCalibs cpc
                , _tcpRange        = s
                , _tcpCorrelate    = corr
                , _tcoObtID        = getDefaultInt _cpcObtID
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
    -> Either Text TCParamDefaultValue
determineDefaultValue epoch coeff ptc pfc inter dispFormat radix val
    | ST.null val = Right TCParamNothing
    | otherwise = case inter of
        'E' -> TCParamEng <$> getVal dispFormat radix val
        _   -> TCParamRaw <$> determineRawDefaultValue epoch coeff ptc pfc val


determineRawDefaultValue
    :: Epoch
    -> CorrelationCoefficients
    -> PTC
    -> PFC
    -> ShortText
    -> Either Text Value
determineRawDefaultValue epoch coeff ptc pfc val =
    case parseShortTextToValue ptc pfc val of
        Left err -> Left err
        Right v ->
            let rawVal = initialValue BiE ptc pfc
            in
                Right $ case _tmvalValue v of
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
        Left  err -> Left err
        Right v   -> Right (RangeDiscrete v)
    else runExcept $ do
        v1 <- liftEither
            $ getVal (getDefaultChar _prfDispFormat) radix _prvMinVal
        v2 <- liftEither
            $ getVal (getDefaultChar _prfDispFormat) radix _prvMaxVal
        return $ RangeMinMax v1 v2
    where radix = charToRadix . getDefaultChar $ _prfRadix



