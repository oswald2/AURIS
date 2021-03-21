module Data.Conversion.Criteria
    ( convertCalibCriteria
    , determineCalibCriterias
    , getCalibCriteria
    )
where

import           RIO
import qualified RIO.HashMap                   as HM
import qualified RIO.Vector                    as V
--import           Data.Either
import           Data.Text.Short                ( ShortText
                                                , toText
                                                )
import           Data.TM.Calibration

import           Data.MIB.CUR



convertCalibCriteria
    :: HashMap ShortText Calibration -> CURentry -> Either Text CritCalib
convertCalibCriteria hm CURentry {..} = case HM.lookup _curSelect hm of
    Nothing ->
        Left
            $  "CalibCriteria: Calibration "
            <> toText _curSelect
            <> " has not been found for parmeter " <> toText _curParamName
    Just calib -> Right CritCalib { _ccRLChk       = _curRlChk
                                  , _ccValPar      = _curValPar
                                  , _ccCalibration = calib
                                  }

determineCalibCriterias
    :: ShortText
    -> Vector CURentry
    -> HashMap ShortText Calibration
    -> Either Text (Vector CritCalib)
determineCalibCriterias paramName curs hm =
    let curs' = V.filter f curs
        f x = _curParamName x == paramName
        crits = V.map (convertCalibCriteria hm) curs'
    in if V.all isRight crits
            then Right . force . V.fromList . rights . toList $ crits
            else
                Left
                $ V.foldl' (\x y -> x <> "\n" <> fromLeft "" y)
                           "Error on getting calibration criterias: "
                $ V.filter isLeft crits



getCalibCriteria :: ShortText -> ShortText -> Vector CURentry -> HashMap ShortText Calibration -> Either Text CalibContainer
getCalibCriteria paramName calibName curs hm =
    case determineCalibCriterias paramName curs hm of
        Left err -> Left err
        Right crits -> if V.null crits
            then -- we have no criterias, so we have a direct calibration or nothing
                case HM.lookup calibName hm of
                    Nothing -> Right CritNoCalib
                    Just cal -> Right (CritDirect cal)
            else Right (Crit crits)