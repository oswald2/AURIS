{-# LANGUAGE
  OverloadedStrings
#-}
module Main where


import           RIO
import           Data.Text.Short                ( ShortText )
import           Data.TM.TMParameterDef
import           Data.TM.Parameter
import           Data.TM.Value
import           Data.TM.Validity
import           General.Types
import           Data.TM.CalibrationTypes
import           Data.TM.Calibration
import           General.Time


def1 :: TMParameterDef
def1 = TMParameterDef { _fpName              = "VALUE"
                      , _fpDescription       = "Value parameter"
                      , _fpPID               = Nothing
                      , _fpUnit              = ""
                      , _fpType              = ParamInteger 32
                      , _fpWidth             = Nothing
                      , _fpValid             = Just def2
                      , _fpRelated           = Nothing
                      , _fpCalibs            = CritNoCalib
                      , _fpNatur             = NaturRaw
                      , _fpInterpolation     = CalibExtrapolate
                      , _fpStatusConsistency = SCCOff
                      , _fpDecim             = 6
                      , _fpDefaultVal = TMValue (TMValInt 0) clearValidity
                      , _fpSubsys            = ""
                      , _fpValidityValue = TMValue (TMValInt 0) clearValidity
                      , _fpOBTID             = Nothing
                      , _fpEndian            = BiE
                      }

def2 :: TMParameterDef
def2 = TMParameterDef { _fpName              = "DEP"
                      , _fpDescription       = "Validity Parameter"
                      , _fpPID               = Nothing
                      , _fpUnit              = ""
                      , _fpType              = ParamInteger 32
                      , _fpWidth             = Nothing
                      , _fpValid             = Nothing
                      , _fpRelated           = Nothing
                      , _fpCalibs            = CritNoCalib
                      , _fpNatur             = NaturRaw
                      , _fpInterpolation     = CalibExtrapolate
                      , _fpStatusConsistency = SCCOff
                      , _fpDecim             = 6
                      , _fpDefaultVal = TMValue (TMValInt 0) clearValidity
                      , _fpSubsys            = ""
                      , _fpValidityValue = TMValue (TMValInt 0) clearValidity
                      , _fpOBTID             = Nothing
                      , _fpEndian            = BiE
                      }

definitions :: [TMParameterDef]
definitions = [def1, def2]

values :: [(ShortText, Int64, Word64)]
values =
  [ ("DEP"  , 0 , 0)
  , ("VALUE", 10, 1)
  , ("VALUE", 15, 5)
  , ("VALUE", 5 , 10)
  , ("DEP"  , 1 , 20)
  , ("VALUE", 20, 22)
  , ("VALUE", 25, 24)
  , ("VALUE", 35, 25)
  ]

conv :: (ShortText, Int64, Word64) -> TMParameter
conv (name, val, timestamp) = TMParameter
  { _pName     = name
  , _pTime     = word64ToTime timestamp False
  , _pValue    = TMValue (TMValInt val) clearValidity
  , _pEngValue = Nothing
  }

paramValues :: [TMParameter]
paramValues = map conv values 


main :: IO ()
main = do
  return ()
