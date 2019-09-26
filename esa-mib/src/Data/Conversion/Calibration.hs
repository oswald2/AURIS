{-# LANGUAGE OverloadedStrings
    , BangPatterns
    , GeneralizedNewtypeDeriving
    , DeriveGeneric
    , RecordWildCards
    , NoImplicitPrelude
    , BinaryLiterals
    , NumericUnderscores
    , FlexibleInstances
    , GADTs
    , ExistentialQuantification
#-}
module Data.Conversion.Calibration
  ( convertNumCalib
  )
where

import           RIO
import qualified RIO.Text                      as T
import qualified RIO.Vector                    as V
import           RIO.List                       ( sortBy
                                                , intersperse
                                                )

import           Data.MIB.CAF
import           Data.MIB.CAP

import           Data.TM.CalibrationTypes
import           Data.TM.NumericalCalibration


-- | Convert from MIB structure to a TM model structure. In this case 
-- the standard numerical calibration is converted. 
-- In case there is an error in the conversion, a 'Left' error message
-- is provided. 
convertNumCalib
  :: CAFentry -> Vector CAPentry -> Either Text NumericalCalibration
convertNumCalib CAFentry {..} caps =
  let points = getCaps
  in  if all isRight points
        then
          let points' = rights points
          in  Right
                (NumericalCalibration
                  { _calibNName          = _cafNumbr
                  , _calibNDescr         = _cafDescr
                  , _calibNInterpolation = toCalibInterpolation _cafInter
                  , _calibNPoints        = V.fromList points'
                  }
                )
        else Left $ T.concat . intersperse "\n" $ (lefts points)
 where
  getCaps =
    let v1 = map conv . sortBy s . filter f $ V.toList caps
        f x = _capNumbr x == _cafNumbr
        s x y = compare (_capXVals x) (_capXVals y)
        conv CAPentry {..} =
            let x = parseShortTextToDouble (charToType _cafRawFmt)
                                           (charToRadix _cafRadix)
                                           _capXVals
                y = parseShortTextToDouble (charToType _cafEngFmt)
                                           Decimal
                                           _capYVals
                chk (Left e)   _          = Left e
                chk _          (Left  e ) = Left e
                chk (Right x1) (Right y1) = Right $ CalibPoint x1 y1
            in  chk x y
    in  v1
