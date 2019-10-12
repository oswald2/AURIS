module Data.Conversion.Parameter
  ( convertParameter
  )
where

import           RIO

import           Data.MIB.PCF

import           Data.TM.TMParameterDef



convertParameter :: PCFentry -> Either Text TMParameterDef
convertParameter PCFentry {..} = 
    let 
        calibs = []
        corr = case _pcfCorr of 
            DefaultToChar 'Y' -> Just True 
            DefaultToChar 'N' -> Just False 
    in runExceptT $ do 
        typ <- except ptcPfcToParamType (PTC _pcfPTC) (PFC _pcfPFC) corr 
        pure (TMParameterDef {
            _fpName = _pcfName
            , _fpDescription = _pcfDescr
            , _fpPID = _pcfPID
            , _fpUnit = _pcfUnit
            , _fpType = typ
            , _fpWidth = _pcfWidth
            , _fpValid = Nothing 
            , _fpRelated = Nothing
            , _fpCalibs = calibs
            , _fpNatur :: !ParamNatur
            , _fpInterpolation :: !InterpolationType
            , _fpStatusConsistency :: !StatusConsistency
            , _fpDecim :: !Int
            , _fpDefaultVal :: !TMValue
            , _fpSubsys :: !ShortText
            , _fpValidityValue :: !TMValue
            , _fpOBTID :: Maybe Int
            , _fpEndian :: Endian
            })