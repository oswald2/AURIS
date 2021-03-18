module Data.Conversion.TCParameter
    ( convertCPC
    , convertPRF
    , convertRange
    ) where

import           RIO
import qualified RIO.Vector                    as V
import qualified RIO.Text                      as T
import qualified RIO.HashMap                   as HM

import qualified Data.Text.Short               as ST
import           RIO.List                       ( intersperse )

import           Data.Text.Short                ( ShortText )
import qualified Data.Text.Short               as ST
import           Control.Monad.Except


import           Data.PUS.Parameter
import           Data.PUS.Value
import           Data.PUS.EncTime

import           General.Types
import           General.Time
import           General.PUSTypes

import           Data.TC.TCParameterDef
import           Data.TC.RangeSet
import           Data.TC.Calibration

import           Data.TM.Value

import           Data.MIB.Types
import           Data.MIB.CPC
import           Data.MIB.PRV
import           Data.MIB.PRF
import           Data.Conversion.Types


convertCPC
    :: Epoch
    -> CorrelationCoefficients
    -> CPCentry
    -> HashMap ShortText PRFentry
    -> Vector PRVentry
    -> Either Text TCParameterDef
convertCPC epoch coeff CPCentry {..} prfMap prvVec =
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
        (ptc, pfc, t) = case getDefaultChar _cpcCateg of
            'A' -> (PTC 7, PFC 0, TCParamCmdID)
            'P' -> (PTC 3, PFC _cpcPFC, TCParamParamID)
            _   -> (PTC _cpcPTC, PFC _cpcPFC, TCParamNormal)
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
                , _tcpCalib        = Nothing
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



convertPRF :: PRFentry -> Vector PRVentry -> Either Text RangeSet
convertPRF prf@PRFentry {..} vec =
    let (errs, prvs) =
            partitionEithers
                . V.toList
                . V.map (convertRange prf)
                . V.filter (\x -> _prvNumbr x == _prfNumbr)
                $ vec
    in  if not (null errs)
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



