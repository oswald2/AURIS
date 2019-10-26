module Data.Conversion.TMPacket
    ( convertPacket
    , convertPackets
    , charToPIDEvent
    , picSeachIndexFromPIC
    )
where

import           RIO
import qualified RIO.HashMap                   as HM
import qualified RIO.List                      as L
import qualified RIO.Text                      as T
import qualified RIO.Vector                    as V
import           Data.Text.Short                ( ShortText )
import qualified Data.Text.Short               as ST

import           Data.HashTable.ST.Basic        ( IHashTable )
import qualified Data.HashTable.ST.Basic       as HT

import           Data.MIB.PID
import           Data.MIB.TPCF
import           Data.MIB.PLF
import           Data.MIB.PIC
import           Data.MIB.Types

import           Data.TM.TMPacketDef
import           Data.TM.TMParameterDef
import           Data.TM.PIVals

import           General.Types
import           General.PUSTypes
import           General.TriState
import           General.APID
import           General.Time
import           General.TimeSpan

import           Data.Conversion.Types





convertPackets
    :: HashMap Word32 TPCFentry
    -> Vector PLFentry
    -> IHashTable ShortText TMParameterDef
    -> Vector PIDentry
    -> Either Text (Warnings, [TMPacketDef])
convertPackets tpcfs plfs paramHT =
    handleTriState . map (convertPacket tpcfs plfs paramHT) . V.toList


convertPacket
    :: HashMap Word32 TPCFentry
    -> Vector PLFentry
    -> IHashTable ShortText TMParameterDef
    -> PIDentry
    -> TriState Text Text (Warnings, TMPacketDef)
convertPacket tpcfs plfs paramHT pid@PIDentry {..} =
    case if _pidTPSD == -1 then getFixedParams else getVariableParams of
        TError err -> TError err
        TWarn  err -> TWarn err
        TOk (warnings, params) ->
            createPacket pid (HM.lookup _pidSPID tpcfs) warnings params

  where
    createPacket PIDentry {..} tpcf warnings params =
        let name = case tpcf of
                Just TPCFentry {..} -> _tpcfName
                Nothing             -> ""
        in
            TOk
                ( warnings
                , TMPacketDef
                    { _tmpdSPID    = SPID _pidSPID
                    , _tmpdName    = name
                    , _tmpdDescr   = _pidDescr
                    , _tmpdType    = mkPUSType (fromIntegral _pidType)
                    , _tmpdSubType = mkPUSSubType (fromIntegral _pidSubType)
                    , _tmpdApid    = APID (fromIntegral _pidAPID)
                    , _tmpdPI1Val  = _pidP1Val
                    , _tmpdPI2Val  = _pidP2Val
                    , _tmpdUnit    = _pidUnit
                    , _tmpdTime    = getPidTime pid
                    , _tmpdInter   = toSpan _pidInter
                    , _tmpdValid   = charDefaultToBool _pidValid
                    , _tmpdCheck   = getDefaultInt _pidCheck /= 0
                    , _tmpdEvent   = charToPIDEvent (getDefaultChar _pidEvent)
                                                    _pidEventID
                    , _tmpdParams  = params
                    }
                )

    toSpan :: Maybe Int -> Maybe (TimeSpn MilliSeconds)
    toSpan = fmap (mkTimeSpan MilliSeconds . fromIntegral)



    getFixedParams =
        let (errs, pls) =
                    partitionEithers
                        . map (convertPacketLocation paramHT)
                        . L.sort
                        . filter (\x -> _pidSPID == _plfSPID x)
                        . toList
                        $ plfs
        in  if null errs
                then TOk (Nothing, TMFixedParams (V.fromList pls))
                else TError (T.concat (L.intersperse ("\n" :: Text) errs))


    getVariableParams = TWarn "Not implemented"




convertPacketLocation
    :: IHashTable ShortText TMParameterDef
    -> PLFentry
    -> Either Text TMParamLocation
convertPacketLocation ht plf@PLFentry {..} = case HT.ilookup ht _plfName of
    Just x -> Right TMParamLocation
        { _tmplName      = _plfName
        , _tmplOffset    =
            toBitOffset (mkOffset (ByteOffset _plfOffBy) (BitOffset _plfOffBi))
        , _tmplTime = fromMilli (fromIntegral (getDefaultInt _plfTime)) True
        , _tmplSuperComm = convertSuperComm plf
        , _tmplParam     = x
        }
    Nothing ->
        Left
            $  "PLF: Parameter "
            <> ST.toText _plfName
            <> " defined in plf.dat not found"


convertSuperComm :: PLFentry -> Maybe SuperCommutated
convertSuperComm PLFentry {..} = case (_plfNbOcc, _plfLgOcc, _plfTdOcc) of
    (Just n, Just lg, Just td) -> Just SuperCommutated
        { _scNbOcc = n
        , _scLgOcc = mkBitSize lg
        , _scTdOcc = mkTimeSpan MilliSeconds (fromIntegral td)
        }
    _ -> Nothing



charToPIDEvent :: Char -> ShortText -> PIDEvent
charToPIDEvent 'N' _    = PIDNo
charToPIDEvent 'I' evid = PIDInfo evid
charToPIDEvent 'W' evid = PIDWarning evid
charToPIDEvent 'A' evid = PIDAlarm evid
charToPIDEvent _   _    = PIDNo



picVecToCriteria :: Vector PICentry -> Vector PacketIDCriteria
picVecToCriteria = V.map conv
  where
    conv pic@PICentry {..} = PacketIDCriteria
        { _pidcAPID    = APID <$> _picApid
        , _pidcType    = mkPUSType _picType
        , _pidcSubType = mkPUSSubType _picSubType
        , _pidcPIs     = convertPIC pic
        }


picSeachIndexFromPIC :: Vector PICentry -> PICSearchIndex
picSeachIndexFromPIC pics =
    let p = picVecToCriteria pics in mkPICSearchIndex p


convertPIC :: PICentry -> TMPIDefs
convertPIC PICentry {..} =
    let
        pi1 = if _picPI1Off == -1
            then Nothing
            else
                Just
                    (TMPIDef (mkByteOffset _picPI1Off)
                             (mkBitSize (fromIntegral _picPI1Width))
                    )
        pi2 = if _picPI2Off == -1
            then Nothing
            else
                Just
                    (TMPIDef (mkByteOffset _picPI2Off)
                             (mkBitSize (fromIntegral _picPI2Width))
                    )
    in
        TMPIDefs pi1 pi2


