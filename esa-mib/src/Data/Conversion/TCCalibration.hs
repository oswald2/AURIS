module Data.Conversion.TCCalibration
    ( convertTcNumCalib
    , convertTcTextCalib
    , convertCCA
    , convertPAF
    ) where


import           RIO
import qualified RIO.Vector                    as V
import qualified RIO.HashMap                   as HM
import           RIO.List                       ( intersperse )

import           Data.Text.Short                ( ShortText )
import           Control.Monad.Except

import           Data.TC.Calibration

import           General.Types

import           Data.MIB.CCA
import           Data.MIB.CCS
import           Data.MIB.PAS
import           Data.MIB.PAF
import           Data.MIB.Types

import           Data.TM.Value

import           Data.Conversion.Types

import qualified Data.Bimap                    as BM
import           Data.Multimap                  ( Multimap )
import qualified Data.Multimap                 as M




convertTcNumCalib
    :: Vector CCAentry
    -> Multimap ShortText CCSentry
    -> ([Text], HashMap ShortText TCNumericCalibration)
convertTcNumCalib vec ccss =
    let (errs, cca) =
            partitionEithers . V.toList . V.map (convertCCA ccss) $ vec
        hm = foldl' (\h x -> HM.insert (_tcncName x) x h) HM.empty cca
    in  (errs, hm)

convertCCA
    :: Multimap ShortText CCSentry
    -> CCAentry
    -> Either Text TCNumericCalibration
convertCCA m cca@CCAentry {..} =
    let (errs, ccs) =
            partitionEithers . map (convertNumVal cca) . M.lookup _ccaNumbr $ m
        ccsMap = BM.fromList ccs
    in  if not (RIO.null errs)
            then
                Left
                $  "Error converting CCSs: "
                <> (mconcat . intersperse "\n") errs
            else Right $ TCNumericCalibration { _tcncName   = _ccaNumbr
                                              , _tcncDescr  = _ccaDescr
                                              , _tcncUnit   = _ccaUnit
                                              , _tcncValues = ccsMap
                                              }


convertNumVal
    :: CCAentry -> CCSentry -> Either Text (TMValueSimple, TMValueSimple)
convertNumVal CCAentry {..} CCSentry {..} = runExcept $ do
    eng <- liftEither $ getVal (getDefaultChar _ccaEngFmt) Decimal _ccsEng
    raw <- liftEither $ getVal (getDefaultChar _ccaRawFmt) radix _ccsRaw
    return (raw, eng)
    where radix = charToRadix . getDefaultChar $ _ccaRadix



convertTcTextCalib
    :: Vector PAFentry
    -> Multimap ShortText PASentry
    -> ([Text], HashMap ShortText TCTextCalibration)
convertTcTextCalib vec pass =
    let (errs, pas) =
            partitionEithers . V.toList . V.map (convertPAF pass) $ vec
        hm = foldl' (\h x -> HM.insert (_tcvName x) x h) HM.empty pas
    in  (errs, hm)



convertPAF
    :: Multimap ShortText PASentry -> PAFentry -> Either Text TCTextCalibration
convertPAF m paf@PAFentry {..} =
    let (errs, pas) =
            partitionEithers . map (convertPASVal paf) . M.lookup _pafNumbr $ m
        pasMap = BM.fromList pas
    in  if not (RIO.null errs)
            then
                Left
                $  "Error converting CCSs: "
                <> (mconcat . intersperse "\n") errs
            else Right $ TCTextCalibration { _tcvName   = _pafNumbr
                                           , _tcvDescr  = _pafDescr
                                           , _tcvValues = pasMap
                                           }



convertPASVal :: PAFentry -> PASentry -> Either Text (TMValueSimple, ShortText)
convertPASVal PAFentry {..} PASentry {..} =
    case getVal (getDefaultChar _pafRawFmt) Decimal _pasRaw of
        Left  err -> Left err
        Right raw -> Right (raw, _pasEng)
