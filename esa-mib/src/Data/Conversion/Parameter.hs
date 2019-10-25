module Data.Conversion.Parameter
    ( convertParameter
    , convertParameters
    )
where

import           RIO
import qualified RIO.HashMap                   as HM
import qualified RIO.Text                      as T
import           RIO.List                       ( intersperse )
import           RIO.Partial                    ( fromJust )
import           Data.Text.Short                ( ShortText
                                                , toText
                                                )
import qualified Data.Text.Short               as ST
import           Data.HashTable.ST.Basic        ( IHashTable )
import qualified Data.HashTable.ST.Basic       as HT

import           Data.MIB.PCF
import           Data.MIB.CUR

import           Data.TM.TMParameterDef
import           Data.TM.Calibration
import           Data.TM.CalibrationTypes
import           Data.TM.Synthetic
import           Data.TM.Value
import           Data.TM.Validity

import           Data.MIB.Types

import           Data.Conversion.Criteria
import           Data.Conversion.Types

import           General.PUSTypes
import           General.TriState




convertParameters
    :: Vector PCFentry
    -> Vector CURentry
    -> HashMap ShortText Calibration
    -> HashMap ShortText Synthetic
    -> (Either Text (Warnings, IHashTable ShortText TMParameterDef))
convertParameters pcfs curs calibHM synHM = 
    let params = map (convertParameter curs calibHM synHM (getPCFMap pcfs))
                     (toList pcfs)
        (errs, wrns, ok) = partitionTriState params
    in
    if not (null errs)
      then 
        Left
          .  T.concat
          $  ["Error creating TM parameters: "]
          <> intersperse "\n" errs
      else 
        -- create a hash table
        let warnings' = if not (null wrns) then Just warnMsg else Nothing
            warnMsg =
              T.concat
                  $ [ "WARNING: on creating TM parameters, parameter ignored: "
                    ]
                  <> intersperse "\n" wrns
            warns2 = map (fromJust . fst) . filter (isJust . fst) $ ok
            warnMsg2 =
              T.concat
                  $  ["WARNING: on creating TM parameters: "]
                  <> intersperse "\n" warns2
            warnings = warnings' <> if not (null warns2)
              then Just "\n" <> Just warnMsg2
              else Nothing
        in
        -- now insert the parameters
        runST $ do 
          ht <- HT.new
          foldM_ ins ht ok
          iht <- HT.unsafeFreeze ht 
          return $ Right (warnings, iht)
  where
    -- insert a parameter definition into the 'HashMap'.
    -- When there are dependent parameters, also insert them
    -- into the HashMap first.
    ins ht (_, par) = do
      -- we ignore the new values
      case _fpValid par of
        Nothing -> do
          HT.mutate ht (_fpName par) (upd par)
          return ht
        Just dep -> do
            ht1 <- ins ht (Nothing, dep)
            HT.mutate ht1 (_fpName par) (upd par)
            return ht1

    upd x Nothing = (Just x, ())
    upd x (Just old) = (Just old, ())


convertParameter
    :: Vector CURentry
    -> HashMap ShortText Calibration
    -> HashMap ShortText Synthetic
    -> HashMap ShortText PCFentry
    -> PCFentry
    -> TriState Text Text (Warnings, TMParameterDef)
convertParameter curs calibHM synthHM pcfs p@PCFentry {..} =
    case ptcPfcToParamType (PTC _pcfPTC) (PFC _pcfPFC) corr of
        Left  err -> TWarn err
        Right !typ -> getCalibs typ
  where
    !corr = case _pcfCorr of
        CharDefaultTo 'Y' -> Just True
        CharDefaultTo 'N' -> Just False
        _                 -> Just True
    inter x = toCalibInterpolation (getDefaultChar x)
    scc x = charToStatusConsistency (getDefaultChar x)

    validityVal = TMValue
        (TMValInt (fromIntegral (getDefaultInt _pcfValPar)))
        clearValidity

    getCalibs :: ParamType -> TriState Text Text (Warnings, TMParameterDef)
    getCalibs typ = case getCalibCriteria _pcfName _pcfCurTx curs calibHM of
        Left  err    -> TError err
        Right calibs -> getNatur typ calibs

    getNatur !typ !calib = case getParamNatur _pcfName _pcfNatur synthHM of
        TError err   -> TError err
        TWarn  err   -> TWarn err
        TOk    !natur -> case getDefVal typ calib natur of
            Left  err -> TError err
            Right !x   -> TOk x

    getDefVal !typ !calib !natur =
        case
                (if ST.null _pcfParVal
                    then Right nullValue
                    else parseShortTextToValue (PTC _pcfPTC)
                                               (PFC _pcfPFC)
                                               _pcfParVal
                )
            of
                Left  err    -> Left err
                Right !defVal -> createParam typ calib natur defVal

    isDep PCFentry {..} = not . ST.null $ _pcfValid

    createParam !typ !calibs !natur !defVal = if isDep p
        then
            let depP = HM.lookup _pcfValid pcfs
                !res  = maybe (TOk (Nothing, param Nothing))
                             (convertParameter curs calibHM synthHM pcfs)
                             depP
            in  case res of
                    TError err -> Left err
                    TWarn err -> Right (Just err, param Nothing)
                    TOk (warns', !valPar) -> Right (warns', param (Just valPar))
        else Right (Nothing, param Nothing)
      where
        param valPar = TMParameterDef { _fpName              = _pcfName
                                      , _fpDescription       = _pcfDescr
                                      , _fpPID               = _pcfPID
                                      , _fpUnit              = _pcfUnit
                                      , _fpType              = typ
                                      , _fpWidth             = _pcfWidth
                                      , _fpValid             = valPar
                                      , _fpRelated           = Nothing
                                      , _fpCalibs            = calibs
                                      , _fpNatur             = natur
                                      , _fpInterpolation     = inter _pcfInter
                                      , _fpStatusConsistency = scc _pcfUscon
                                      , _fpDecim = fromMaybe 0 _pcfDecim
                                      , _fpDefaultVal        = defVal
                                      , _fpSubsys            = _pcfSubSys
                                      , _fpValidityValue     = validityVal
                                      , _fpOBTID             = _pcfOBTID
                                      , _fpEndian            = getEndian p
                                      }


getParamNatur
    :: ShortText
    -> Char
    -> HashMap ShortText Synthetic
    -> TriState Text Text ParamNatur
getParamNatur paramName t synthHM
    | t == 'R' = TOk NaturRaw
    | t == 'D' || t == 'H' || t == 'S' = case HM.lookup paramName synthHM of
        Nothing ->
            TWarn
                $  "Error: no synthetic definition found for parameter: "
                <> toText paramName
        Just syn -> TOk $ NaturSynthetic syn
    | t == 'P' = TWarn "SPEL synthetic parameters not yet implemented"
    | t == 'C' = TOk NaturConstant
    | otherwise = TError $ "Illegal PCF_NATUR: " <> T.singleton t
