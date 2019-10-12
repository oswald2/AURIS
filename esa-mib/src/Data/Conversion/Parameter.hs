module Data.Conversion.Parameter
    ( convertParameter
    , convertParameters
    )
where

import           RIO
import qualified RIO.HashMap                   as HM
import qualified RIO.Text                      as T
import           RIO.List                       ( intersperse )
import           Data.Text.Short                ( ShortText
                                                , toText
                                                )

import           Data.MIB.PCF
import           Data.MIB.CUR

import           Data.TM.TMParameterDef
import           Data.TM.Calibration
import           Data.TM.CalibrationTypes
import           Data.TM.Synthetic
import           Data.TM.Value
import           Data.TM.Validity

import           Data.MIB.Types

import           Data.Conversion.Criteria

import           Control.Monad.Except

import           General.PUSTypes



convertParameters
    :: Vector PCFentry
    -> Vector CURentry
    -> HashMap ShortText Calibration
    -> HashMap ShortText Synthetic
    -> Either Text (HashMap ShortText TMParameterDef)
convertParameters pcfs curs calibHM synHM =
    let params = map (convertParameter curs calibHM synHM) (toList pcfs)
    in  if all isRight params
            then Right $ foldl' (\hm x -> HM.insert (_fpName x) x hm)
                                HM.empty
                                (rights params)
            else
                Left
                .  T.concat
                $  ["Error creating TM parameters: "]
                <> intersperse "\n" (lefts params)



convertParameter
    :: Vector CURentry
    -> HashMap ShortText Calibration
    -> HashMap ShortText Synthetic
    -> PCFentry
    -> Either Text TMParameterDef
convertParameter curs calibHM synthHM p@PCFentry {..} =
    let
        corr = case _pcfCorr of
            CharDefaultTo 'Y' -> Just True
            CharDefaultTo 'N' -> Just False
            _                 -> Just True
        inter x = toCalibInterpolation (getDefaultChar x)
        scc x = charToStatusConsistency (getDefaultChar x)
        validityVal = TMValue
            (TMValInt (fromIntegral (getDefaultInt _pcfValPar)))
            clearValidity
    in
        runExcept $ do
            typ <- liftEither
                $ ptcPfcToParamType (PTC _pcfPTC) (PFC _pcfPFC) corr
            calibs <- liftEither
                $ getCalibCriteria _pcfName _pcfCurTx curs calibHM
            natur  <- liftEither $ getParamNatur _pcfName _pcfNatur synthHM
            defVal <- liftEither
                $ parseShortTextToValue (PTC _pcfPTC) (PFC _pcfPFC) _pcfParVal
            pure TMParameterDef { _fpName              = _pcfName
                                , _fpDescription       = _pcfDescr
                                , _fpPID               = _pcfPID
                                , _fpUnit              = _pcfUnit
                                , _fpType              = typ
                                , _fpWidth             = _pcfWidth
                                , _fpValid             = Nothing
                                , _fpRelated           = Nothing
                                , _fpCalibs            = calibs
                                , _fpNatur             = natur
                                , _fpInterpolation     = inter _pcfInter
                                , _fpStatusConsistency = scc _pcfUscon
                                , _fpDecim             = fromMaybe 0 _pcfDecim
                                , _fpDefaultVal        = defVal
                                , _fpSubsys            = _pcfSubSys
                                , _fpValidityValue     = validityVal
                                , _fpOBTID             = _pcfOBTID
                                , _fpEndian            = getEndian p
                                }


getParamNatur
    :: ShortText
    -> Char
    -> HashMap ShortText Synthetic
    -> Either Text ParamNatur
getParamNatur paramName t synthHM
    | t == 'R' = Right NaturRaw
    | t == 'D' || t == 'H' || t == 'S' = case HM.lookup paramName synthHM of
        Nothing ->
            Left
                $  "Error: no synthetic definition found for parameter: "
                <> toText paramName
        Just syn -> Right $ NaturSynthetic syn
    | t == 'P' = Left "SPEL synthetic parameters not yet implemented"
    | t == 'C' = Right NaturConstant
    | otherwise = Left $ "Illegal PCF_NATUR: " <> T.singleton t
