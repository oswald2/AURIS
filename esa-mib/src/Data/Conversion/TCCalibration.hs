module Data.Conversion.TCCalibration
    ( convertCCA
    , convertPAF
    ) where


import           RIO
import qualified RIO.Vector                    as V
import           RIO.List                       ( intersperse )

import           Control.Monad.Except

import           Data.TC.Calibration

import           General.Types

import           Data.MIB.CCA
import           Data.MIB.CCS
import           Data.MIB.PAS
import           Data.MIB.PAF
import           Data.MIB.Types

import           Data.Conversion.Types








convertCCA :: CCAentry -> Vector CCSentry -> Either Text TCNumericCalibration
convertCCA cca@CCAentry {..} vec =
    let (errs, ccs) =
            partitionEithers
                . V.toList
                . V.map (convertNumVal cca)
                . V.filter (\x -> _ccsNumbr x == _ccaNumbr)
                $ vec
    in  if not (null errs)
            then
                Left
                $  "Error converting CCSs: "
                <> (mconcat . intersperse "\n") errs
            else Right $ TCNumericCalibration { _tcncName   = _ccaNumbr
                                              , _tcncDescr  = _ccaDescr
                                              , _tcncUnit   = _ccaUnit
                                              , _tcncValues = V.fromList ccs
                                              }


convertNumVal :: CCAentry -> CCSentry -> Either Text NumCalibVal
convertNumVal CCAentry {..} CCSentry {..} = runExcept $ do
    eng <- liftEither $ getVal (getDefaultChar _ccaEngFmt) Decimal _ccsEng
    raw <- liftEither $ getVal (getDefaultChar _ccaRawFmt) radix _ccsRaw
    return $ NumCalibVal { _ncvRaw = raw, _ncvEng = eng }
    where radix = charToRadix . getDefaultChar $ _ccaRadix




convertPAF :: PAFentry -> Vector PASentry -> Either Text TCTextCalibration
convertPAF paf@PAFentry {..} vec =
    let (errs, pas) =
            partitionEithers
                . V.toList
                . V.map (convertPASVal paf)
                . V.filter (\x -> _pasNumbr x == _pafNumbr)
                $ vec
    in  if not (null errs)
            then
                Left
                $  "Error converting CCSs: "
                <> (mconcat . intersperse "\n") errs
            else Right $ TCTextCalibration { _tcvName   = _pafNumbr
                                           , _tcvDescr  = _pafDescr
                                           , _tcvValues = V.fromList pas
                                           }



convertPASVal :: PAFentry -> PASentry -> Either Text TextCalibVal
convertPASVal PAFentry {..} PASentry {..} =
    case getVal (getDefaultChar _pafRawFmt) Decimal _pasRaw of
        Left  err -> Left err
        Right raw -> Right $ TextCalibVal { _tcvRaw = raw, _tcvEng = _pasEng }
