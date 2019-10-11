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
  , convertPolyCalib
  , convertLogCalib
  , convertTextCalib
  )
where

import           RIO
import qualified RIO.Text                      as T
import qualified RIO.Vector                    as V
import           RIO.List                       ( sortBy
                                                , intersperse
                                                )
import           Data.Text.Short                ( toText )

import           Data.MIB.Types
import           Data.MIB.CAF
import           Data.MIB.CAP
import           Data.MIB.MCF
import           Data.MIB.LGF
import           Data.MIB.TXF
import           Data.MIB.TXP

import           Data.TM.CalibrationTypes
import           Data.TM.NumericalCalibration
import           Data.TM.PolynomialCalibration
import           Data.TM.LogarithmicCalibration
import           Data.TM.TextualCalibration

import           Control.Monad.Trans.Except




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
        else
          Left
          $  T.concat
          .  intersperse "\n"
          $  ["Conversion to NumericalCalibration " <> toText _cafNumbr]
          <> lefts points
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


convertTextCalib
  :: TXFentry -> Vector TXPentry -> Either Text TextualCalibration
convertTextCalib TXFentry {..} vec =
  let points = getPoints rawFmt
      rawFmt = charToType _txfRawFmt
  in  if all isRight points
        then
          let points' = rights points
          in  Right $ TextualCalibration { _calibTName   = _txfNumbr
                                         , _calibTDescr  = _txfDescr
                                         , _calibTRawFmt = rawFmt
                                         , _calibTPoints = V.fromList points'
                                         }
        else
          Left
          $  T.concat
          .  intersperse "\n"
          $  ["Conversion to TextualCalibration " <> toText _txfNumbr]
          <> lefts points
 where
  getPoints rawFmt =
    let v1 = map chk . filter f $ V.toList vec
        f x = _txpNumbr x == _txfNumbr
        chk TXPentry {..} = runExcept $ do
          from <- except $ parseShortTextToInt64 rawFmt Decimal _txpFrom
          to'  <- except $ parseShortTextToInt64 rawFmt Decimal _txpTo
          pure (TextCalibPoint from to' _txpAlTxt)
    in  v1



-- | Convert a 'MCFentry' to the 'PolynomialCalibration' type. Returns
-- an error message if the data cannot be parsed
convertPolyCalib :: MCFentry -> Either Text PolynomialCalibration
convertPolyCalib MCFentry {..} = case conv of
  Left err ->
    Left $ "Conversion to PolynomialCalibration " <> toText _mcfIdent <> err
  Right (a0, a1, a2, a3, a4) -> Right $ PolynomialCalibration
    { _calibPName  = _mcfIdent
    , _calibPDescr = _mcfDescr
    , _pa0         = a0
    , _pa1         = a1
    , _pa2         = a2
    , _pa3         = a3
    , _pa4         = a4
    }
 where
  f    = except . parseShortTextToDouble NumDouble Decimal . getDefaultShortText
  conv = runExcept $ do
    a0 <- f _mcfPol1
    a1 <- f _mcfPol2
    a2 <- f _mcfPol3
    a3 <- f _mcfPol4
    a4 <- f _mcfPol5
    pure (a0, a1, a2, a3, a4)





convertLogCalib :: LGFentry -> Either Text LogarithmicCalibration
convertLogCalib LGFentry {..} = case conv of
  Left err ->
    Left $ "Conversion to LogarithmicCalibration " <> toText _lgfIdent <> err
  Right (a0, a1, a2, a3, a4) -> Right $ LogarithmicCalibration
    { _calibLName  = _lgfIdent
    , _calibLDescr = _lgfDescr
    , _la0         = a0
    , _la1         = a1
    , _la2         = a2
    , _la3         = a3
    , _la4         = a4
    }
 where
  f    = except . parseShortTextToDouble NumDouble Decimal . getDefaultShortText
  conv = runExcept $ do
    a0 <- f _lgfPol1
    a1 <- f _lgfPol2
    a2 <- f _lgfPol3
    a3 <- f _lgfPol4
    a4 <- f _lgfPol5
    pure (a0, a1, a2, a3, a4)



