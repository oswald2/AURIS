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
import           RIO.List                       ( sortBy )
import           Data.Text.Short                ( toText )

import           Data.MIB.Types
import           Data.MIB.CAF
import           Data.MIB.CAP
import           Data.MIB.MCF
import           Data.MIB.LGF
import           Data.MIB.TXF
import           Data.MIB.TXP

import           Data.PUS.CalibrationTypes
import           Data.TM.NumericalCalibration
import           Data.TM.PolynomialCalibration
import           Data.TM.LogarithmicCalibration
import           Data.TM.TextualCalibration
import           Data.TM.Value


-- | Convert from MIB structure to a TM model structure. In this case
-- the standard numerical calibration is converted.
-- In case there is an error in the conversion, a 'Left' error message
-- is provided.
convertNumCalib
    :: CAFentry -> Vector CAPentry -> Either Text NumericalCalibration
convertNumCalib CAFentry {..} caps
    = mapLeft errInfo
    $ NumericalCalibration
        <$> pure _cafNumbr
        <*> pure _cafDescr
        <*> pure (toCalibInterpolation $ getDefaultChar _cafInter)
        <*> (fmap V.fromList) (sequence points)
  where
    errInfo _
        =  T.unlines
        $  ["Conversion to NumericalCalibration " <> toText _cafNumbr]
        <> lefts points
    points = map conv . sortBy s . filter f $ V.toList caps
    f x = _capNumbr x == _cafNumbr
    s x y = compare (_capXVals x) (_capXVals y)
    conv CAPentry {..}
        = CalibPoint
            <$> parseShortTextToDouble
                (charToType _cafRawFmt)
                (charToRadix _cafRadix)
                _capXVals
            <*> parseShortTextToDouble
                (charToType _cafEngFmt)
                Decimal
                _capYVals


convertTextCalib
    :: TXFentry -> Vector TXPentry -> Either Text TextualCalibration
convertTextCalib TXFentry {..} vec
    = mapLeft errInfo
    $ TextualCalibration
        <$> pure _txfNumbr
        <*> pure _txfDescr
        <*> pure rawFmt
        <*> fmap V.fromList (sequence points)
  where
    errInfo _
        =  T.unlines
        $  ["Conversion to TextualCalibration " <> toText _txfNumbr]
        <> lefts points
    points = map toPoint . filter f $ V.toList vec
    f x = _txpNumbr x == _txfNumbr
    toPoint TXPentry {..} = TextCalibPoint
        <$> parseShortTextToInt64 rawFmt Decimal _txpFrom
        <*> parseShortTextToInt64 rawFmt Decimal _txpTo
        <*> pure _txpAlTxt
    rawFmt = charToType _txfRawFmt


-- | Convert a 'MCFentry' to the 'PolynomialCalibration' type. Returns
-- an error message if the data cannot be parsed
convertPolyCalib :: MCFentry -> Either Text PolynomialCalibration
convertPolyCalib MCFentry {..}
    = mapLeft errInfo
    $ PolynomialCalibration
        <$> pure _mcfIdent
        <*> pure _mcfDescr
        <*> f _mcfPol1
        <*> f _mcfPol2
        <*> f _mcfPol3
        <*> f _mcfPol4
        <*> f _mcfPol5
  where
    errInfo err = "Conversion to PolynomialCalibration "
        <> toText _mcfIdent
        <> err
    f = parseShortTextToDouble NumDouble Decimal
        . getDefaultShortText


convertLogCalib :: LGFentry -> Either Text LogarithmicCalibration
convertLogCalib LGFentry {..}
    = mapLeft errInfo
    $ LogarithmicCalibration
        <$> pure _lgfIdent
        <*> pure _lgfDescr
        <*> f _lgfPol1
        <*> f _lgfPol2
        <*> f _lgfPol3
        <*> f _lgfPol4
        <*> f _lgfPol5
  where
    errInfo err = "Conversion to LogarithmicCalibration "
        <> toText _lgfIdent
        <> err
    f = parseShortTextToDouble NumDouble Decimal
        . getDefaultShortText
