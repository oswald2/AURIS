{-|
Module      : Data.TM.NumericalCalibration
Description : Data type for a numerical calibration
Copyright   : (c) Michael Oswald, 2019
License     : BSD-3
Maintainer  : michael.oswald@onikudaki.net
Stability   : experimental
Portability : POSIX

This module handles numerical calibrations of TM parameters
-}
{-# LANGUAGE
    TemplateHaskell
#-}
module Data.TM.NumericalCalibration
    ( CalibPoint(..)
    , NumericalCalibration(..)
    , cx
    , cy
    , calibNName
    , calibNDescr
    , calibNInterpolation
    , calibNPoints
    , numericalCalibrationBuilder
    ) where

import           RIO                     hiding ( (.~) )
import           RIO.Text                      as T
import qualified RIO.Vector                    as V
import           RIO.Vector.Partial             ( (!) )

import           Codec.Serialise
import           Control.Lens                   ( (.~)
                                                , makeLenses
                                                )
import           Data.Aeson

import           Data.Text.Short                ( ShortText )
import qualified Data.Text.Short               as ST

import           Data.PUS.CalibrationTypes
import           Data.TM.Validity        hiding ( isValid )
import           Data.TM.Value

import           Text.Builder                  as TB


-- | A data point to be used within the calibration
data CalibPoint = CalibPoint
    { _cx :: !Double
    , _cy :: !Double
    }
    deriving (Eq, Show, Generic)
makeLenses ''CalibPoint

instance NFData CalibPoint
instance Serialise CalibPoint
instance FromJSON CalibPoint
instance ToJSON CalibPoint where
    toEncoding = genericToEncoding defaultOptions

calibPointBuilder :: CalibPoint -> TB.Builder
calibPointBuilder (CalibPoint x y) =
    text "x: " <> fixedDouble 16 x <> text ", y: " <> fixedDouble 16 y


-- | The numerical calibration. Calibrates a value with a given
-- set of calibration points. The value is interpolated between
-- the given points. The '_calibNInterpolation' specifies what
-- should be done when the given value falls outside of the range
-- of the calibration (see 'CalibInterpolation')
data NumericalCalibration = NumericalCalibration
    { _calibNName          :: !ShortText
    , _calibNDescr         :: !ShortText
    , _calibNInterpolation :: !CalibInterpolation
    , _calibNPoints        :: Vector CalibPoint
    }
    deriving (Eq, Show, Generic)
makeLenses ''NumericalCalibration

instance NFData NumericalCalibration
instance Serialise NumericalCalibration
instance FromJSON NumericalCalibration
instance ToJSON NumericalCalibration where
    toEncoding = genericToEncoding defaultOptions


padBuilder :: Int -> TB.Builder
padBuilder n = text (T.replicate n " ")

newLineBuilder :: Int -> TB.Builder 
newLineBuilder n = char '\n' <> padBuilder n


numericalCalibrationBuilder :: NumericalCalibration -> Int -> TB.Builder
numericalCalibrationBuilder calib indent =
    padBuilder indent <> padFromRight 23 ' ' (text "<b>Name:</b> ")
        <> text (ST.toText (_calibNName calib))
        <> newLineBuilder indent <> padFromRight 23 ' ' (text "<b>Description:</b> ")
        <> text (ST.toText (_calibNDescr calib))
        <> newLineBuilder indent <> padFromRight 23 ' ' (text "<b>Interpolation:</b> ")
        <> calibInterpolationBuilder (_calibNInterpolation calib)
        <> newLineBuilder indent <> text "<b>Points:</b>"
        <> newLineBuilder indent <> text "<b>-------</b>\n"
        <> pointBuilder (_calibNPoints calib) (indent + 4)

pointBuilder :: Vector CalibPoint -> Int -> TB.Builder
pointBuilder v indent = padBuilder indent <> TB.intercalate
    (newLineBuilder indent)
    (RIO.map calibPointBuilder (V.toList v))

-- | This type specifies, if the interpolation is done within an interval
-- (in case the point lies within the calibration curve) or if an
-- extrapolation should be done (when the point is outside the calibration
-- curve). In case the point is left to the calibration curve, the first two
-- points are returned for extrapolation, in case the point is right to the
-- curve, the last two points are returned
data CalibInterval =
    Extrapolate CalibPoint CalibPoint
    | Interpolate CalibPoint CalibPoint


instance Calibrate NumericalCalibration where
    calibrate calib rawValue | isValid rawValue = if isNumeric rawValue
        then
            let inter = findInterval (_calibNPoints calib) x
                x     = toDouble rawValue
            in  case inter of
                  -- regular calibration
                    Interpolate c1 c2 -> TMValue
                        (TMValDouble (interpolate c1 c2 x))
                        (_tmvalValidity rawValue)
                    -- outside lower range of calibration
                    Extrapolate c1 c2 -> case _calibNInterpolation calib of
                        CalibFail ->
                            setValidity rawValue validitySetOutOfCalibRange
                        CalibExtrapolate ->
                            rawValue
                                &  tmvalValue
                                .~ TMValDouble (interpolate c1 c2 x)
                                &  over tmvalValidity validitySetExtrapolated
        else setValidity rawValue validitySetWrongType
    calibrate _calib rawValue = rawValue



findInterval :: Vector CalibPoint -> Double -> CalibInterval
findInterval vec x =
    let v0    = vec ! 0
        vend  = vec ! lasti
        lasti = V.length vec - 1
    in  if
            | x < _cx v0    -> Extrapolate v0 (vec ! 1)
            | x >= _cx vend -> Extrapolate (vec ! (lasti - 1)) vend
            | otherwise     -> go 0 lasti
  where
    check :: CalibPoint -> CalibPoint -> Bool
    check (CalibPoint x1 _) (CalibPoint x2 _) = x1 <= x && x < x2

    go !i lasti
        | i == lasti
        = Extrapolate (vec ! (lasti - 1)) (vec ! lasti)
        | otherwise
        = let vi  = vec ! i
              vi1 = vec ! (i + 1)
          in  if check vi vi1 then Interpolate vi vi1 else go (i + 1) lasti


interpolate :: CalibPoint -> CalibPoint -> Double -> Double
interpolate (CalibPoint x1 y1) (CalibPoint x2 y2) !x =
    (y2 - y1) / (x2 - x1) * (x - x1) + y1



