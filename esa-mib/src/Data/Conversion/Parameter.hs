module Data.Conversion.Parameter
  ( convertParameter
  )
where

import           RIO

import           Data.MIB.PCF

import           Data.TM.TMParameterDef



convertParameter :: PCFentry -> TMParameterDef
convertParameter PCFentry {..} = 
    let result = TMParameterDef {
        _fpName = _pcfName
        , _fpDescription = _pcfDescr
        , _fpPID = _pcfPID
        , _fpUnit = _pcfUnit
        , _fpType = ptcPfcToParamType (PTC _pcfPTC) (PFC _pcfPFC)
        , _fpWidth = _pcfWidth
        , _fpValid :: Maybe TMParameterDef
        , _fpRelated :: Maybe TMParameterDef
        , _fpCalibs :: [Calibration]
        , _fpNatur :: !ParamNatur
        , _fpInterpolation :: !InterpolationType
        , _fpStatusConsistency :: !StatusConsistency
        , _fpDecim :: !Int
        , _fpDefaultVal :: !TMValue
        , _fpSubsys :: !ShortText
        , _fpValidityValue :: !TMValue
        , _fpOBTID :: Maybe Int
        , _fpEndian :: Endian
        }
    in result 
