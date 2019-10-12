module Data.Conversion.Parameter
    ( convertParameter
    )
where

import           RIO
import           Data.Text.Short

import           Data.MIB.PCF
import           Data.MIB.CUR

import           Data.TM.TMParameterDef
import           Data.TM.Calibration

import           Data.MIB.Types

import           Data.Conversion.Criteria

import           Control.Monad.Except

import           General.PUSTypes


convertParameter
    :: PCFentry
    -> Vector CURentry
    -> HashMap ShortText Calibration
    -> Either Text TMParameterDef
convertParameter PCFentry {..} curs hm =
    let corr = case _pcfCorr of
            CharDefaultTo 'Y' -> Just True
            CharDefaultTo 'N' -> Just False
            _                 -> Just True
    in  runExcept $ do
            typ <- liftEither
                $ ptcPfcToParamType (PTC _pcfPTC) (PFC _pcfPFC) corr
            calibs <- liftEither $ getCalibCriteria _pcfName _pcfCurTx curs hm
            pure TMParameterDef { _fpName              = _pcfName
                                , _fpDescription       = _pcfDescr
                                , _fpPID               = _pcfPID
                                , _fpUnit              = _pcfUnit
                                , _fpType              = typ
                                , _fpWidth             = _pcfWidth
                                , _fpValid             = Nothing
                                , _fpRelated           = Nothing
                                , _fpCalibs            = calibs
                                , _fpNatur             = undefined
                                , _fpInterpolation     = undefined
                                , _fpStatusConsistency = undefined
                                , _fpDecim             = undefined
                                , _fpDefaultVal        = undefined
                                , _fpSubsys            = undefined
                                , _fpValidityValue     = undefined
                                , _fpOBTID             = undefined
                                , _fpEndian            = undefined
                -- , _fpNatur :: !ParamNatur
                -- , _fpInterpolation :: !InterpolationType
                -- , _fpStatusConsistency :: !StatusConsistency
                -- , _fpDecim :: !Int
                -- , _fpDefaultVal :: !TMValue
                -- , _fpSubsys :: !ShortText
                -- , _fpValidityValue :: !TMValue
                -- , _fpOBTID :: Maybe Int
                -- , _fpEndian :: Endian
                                }

