{-# LANGUAGE TypeApplications #-}
module Data.PUS.TMPacketProcessing
    ( packetProcessorC
    , getPackeDefinition
    )
where


import           RIO
import qualified RIO.ByteString                as B
import           Conduit
import           Data.HashTable.ST.Basic       as HT
import qualified Data.Text.Short               as ST

import           Control.PUS.Classes
import           Data.DataModel

import           Data.TM.TMPacketDef
import           Data.TM.TMParameterDef
import           Data.TM.PIVals
--import           Data.TM.Parameter
import           Data.TM.Value
import           Data.TM.Validity

import           Data.PUS.ExtractedDU
import           Data.PUS.PUSPacket
import           Data.PUS.PUSDfh
import           Data.PUS.TMPacket
--import           Data.PUS.MissionSpecific.Definitions
import           Data.PUS.EncTime
import           Data.PUS.PUSState

import           General.GetBitField
import           General.Types
import           General.Time



packetProcessorC
    :: ( MonadIO m
       , MonadReader env m
       , HasDataModel env
       , HasCorrelationState env
       , HasPUSState env
       , HasLogFunc env
       )
    => ConduitT (ByteString, ExtractedDU PUSPacket) TMPacket m ()
packetProcessorC = awaitForever $ \pkt@(_, pusPkt) -> do
    model' <- view getDataModel
    model  <- readTVarIO model'
    let def' = getPackeDefinition model pkt
    case def' of
        Just def -> yieldM $ processPacket def pkt
        Nothing  -> do
            logWarn
                $  "No packet defintion found for packet: APID:"
                <> display (pusPkt ^. epDU . pusHdr . pusHdrTcApid)
                <> " Type:"
                <> display (pusType (pusPkt ^. epDU . pusDfh))
                <> " SubType:"
                <> display (pusSubType (pusPkt ^. epDU . pusDfh))



-- | Main function to detect the packet identification of a newly received
-- TM PUS Packet.
getPackeDefinition
    :: DataModel -> (ByteString, ExtractedDU PUSPacket) -> Maybe TMPacketDef
getPackeDefinition model (bytes, pkt) =
    let pusPkt     = pkt ^. epDU
        hdr        = pusPkt ^. pusHdr
        apid       = hdr ^. pusHdrTcApid
        dfh        = pusPkt ^. pusDfh
        t          = pusType dfh
        st         = pusSubType dfh
        (pi1, pi2) = case picFind (model ^. dmPacketIdIdx) apid t st of
            Just def -> extractPIVals def bytes
            Nothing  -> (0, 0)
        pktKey = TMPacketKey apid t st pi1 pi2
    in  HT.ilookup (model ^. dmTMPackets) pktKey




extractPIVals :: TMPIDefs -> ByteString -> (Int64, Int64)
extractPIVals TMPIDefs {..} bytes =
    let pi1 = maybe 0 (extractPIVal bytes) _tmpidP1
        pi2 = maybe 0 (extractPIVal bytes) _tmpidP2
    in  (pi1, pi2)


extractPIVal :: ByteString -> TMPIDef -> Int64
extractPIVal bytes TMPIDef {..}
    | _tmpidWidth == 8 = fromIntegral $ getValue @Int8 bytes _tmpidOffset BiE
    | _tmpidWidth == 16 = fromIntegral $ getValue @Int16 bytes _tmpidOffset BiE
    | _tmpidWidth == 32 = fromIntegral $ getValue @Int32 bytes _tmpidOffset BiE
    | _tmpidWidth == 48 = fromIntegral $ getValue @Int48 bytes _tmpidOffset BiE
    | _tmpidWidth == 64 = getValue @Int64 bytes _tmpidOffset BiE
    | otherwise = fromIntegral
        (getBitField bytes (toOffset _tmpidOffset) _tmpidWidth)


processPacket
    :: (MonadIO m, MonadReader env m, HasPUSState env, HasCorrelationState env)
    => TMPacketDef
    -> (ByteString, ExtractedDU PUSPacket)
    -> m TMPacket
processPacket pktDef pkt = do
    timestamp <- getTimeStamp pkt
    undefined


getTimeStamp
    :: (MonadIO m, MonadReader env m, HasPUSState env, HasCorrelationState env)
    => (ByteString, ExtractedDU PUSPacket)
    -> m SunTime
getTimeStamp (_, epu) = do
    appState <- view appStateG
    state    <- readTVarIO appState

    let epoch = state ^. pusStEpoch

    case pusPktTime (epu ^. epDU . pusDfh) of
        Just time -> do
            let sunTime = cucTimeToSunTime epoch time
            correlateTMTime sunTime (epu ^. epERT)
        Nothing -> do
            case state ^. pusStCorrelation of
                CorrERT -> return (epu ^. epERT)
                _       -> liftIO getCurrentTime



correlateTMTime
    :: (MonadIO m, MonadReader env m, HasPUSState env, HasCorrelationState env)
    => SunTime
    -> SunTime
    -> m SunTime
correlateTMTime inTime ert = do
    appState <- view appStateG
    state    <- readTVarIO appState

    case state ^. pusStCorrelation of
        CorrNo  -> return inTime
        CorrSYS -> return inTime
        CorrERT -> return ert
        CorrPKT -> do
            corrState    <- view corrStateG
            coefficients <- readTVarIO corrState
            return (obtToMcsTim inTime coefficients)



extractParamValue :: Epoch -> ByteString -> TMParamLocation -> (TMValue, Correlate)
extractParamValue epoch oct def =
    let
        par    = def ^. tmplParam
        endian = par ^. fpEndian
    in
        case def ^. tmplParam . fpType of
            ParamInteger w ->
                let v = readIntParam oct (_tmplOffset def) (BitSize w) endian
                in  (TMValue v clearValidity, CorrelationNo)
            ParamUInteger w ->
                let v = readUIntParam oct (_tmplOffset def) (BitSize w) endian
                in  (TMValue v clearValidity, CorrelationNo)
            ParamDouble dt ->
                let v = readDoubleParam oct (_tmplOffset def) endian dt
                in  (TMValue v clearValidity, CorrelationNo)
            ParamString w ->
                let v = readString oct (_tmplOffset def) w
                in  (TMValue v clearValidity, CorrelationNo)
            ParamOctet w ->
                let v = readOctet oct (_tmplOffset def) w
                in (TMValue v clearValidity, CorrelationNo)
            ParamTime tt ct ->
                let v = readTime oct (_tmplOffset def) epoch tt
                in (TMValue v clearValidity, ct)
  where
    readIntParam oct off width endian = if isByteAligned off
        then case width of
            8 ->
                TMValInt
                    (fromIntegral (getValue @Int8 oct (toByteOffset off) endian)
                    )
            16 ->
                TMValInt
                    (fromIntegral
                        (getValue @Int16 oct (toByteOffset off) endian)
                    )
            32 ->
                TMValInt
                    (fromIntegral
                        (getValue @Int32 oct (toByteOffset off) endian)
                    )
            64 -> TMValInt (getValue @Int64 oct (toByteOffset off) endian)
            w  -> TMValInt (getBitFieldInt64 oct (_tmplOffset def) w endian)
        else
            TMValInt
                (fromIntegral
                    (getBitFieldInt64 oct (_tmplOffset def) width endian)
                )
    readUIntParam oct off width endian = if isByteAligned off
        then case width of
            8 ->
                TMValUInt
                    (fromIntegral
                        (getValue @Word8 oct (toByteOffset off) endian)
                    )
            16 ->
                TMValUInt
                    (fromIntegral
                        (getValue @Word16 oct (toByteOffset off) endian)
                    )
            32 ->
                TMValUInt
                    (fromIntegral
                        (getValue @Word32 oct (toByteOffset off) endian)
                    )
            64 -> TMValUInt (getValue @Word64 oct (toByteOffset off) endian)
            w  -> TMValUInt (getBitFieldWord64 oct (_tmplOffset def) w endian)
        else TMValUInt (getBitFieldWord64 oct (_tmplOffset def) width endian)


    readDoubleParam oct off endian DTDouble = if isByteAligned off
        then TMValDouble (getValue @Double oct (toByteOffset off) endian)
        else TMValDouble (getBitFieldDouble oct (toOffset off) endian)
    readDoubleParam oct off endian DTFloat = if isByteAligned off
        then TMValDouble
            (realToFrac (getValue @Float oct (toByteOffset off) endian))
        else TMValDouble
            (realToFrac (getBitFieldFloat oct (toOffset off) endian))
    readDoubleParam oct off endian DTMilSingle = if isByteAligned off
        then
            TMValDouble
                (getMilSingle
                    (getValue @MILSingle oct (toByteOffset off) endian)
                )
        else TMValDouble (getBitFieldMilSingle oct (toOffset off) endian)
    readDoubleParam oct off endian DTMilExtended = if isByteAligned off
        then TMValDouble
            (getMilExtended
                (getValue @MILExtended oct (toByteOffset off) endian)
            )
        else TMValDouble (getBitFieldMilExtended oct (toOffset off) endian)

    readString oct off (Just w) =
        let val = B.take w $ B.drop (unByteOffset (toByteOffset off)) oct
        in  case ST.fromByteString val of
                Just x  -> TMValString x
                Nothing -> TMValString "UNDEFINED"
    readString oct off Nothing =
        let val = B.drop (unByteOffset (toByteOffset off)) oct
        in  case ST.fromByteString val of
                Just x  -> TMValString x
                Nothing -> TMValString "UNDEFINED"

    readOctet oct off (Just w) =
        let val = B.take w $ B.drop (unByteOffset (toByteOffset off)) oct
        in TMValOctet val
    readOctet oct off Nothing =
        let val = B.drop (unByteOffset (toByteOffset off)) oct
        in TMValOctet val

    readTime oct off epoch (CUC4Coarse2Fine delta) =
        let !sec = getValue @Int32 oct (toByteOffset off) BiE
            !mic = getValue @Int16 oct (toByteOffset off + 4) BiE
            !encTime = mkCUCTime (fromIntegral sec) (fromIntegral mic) delta
            st = cucTimeToSunTime epoch encTime
        in
        TMValTime st
    readTime oct off epoch (CDS8 delta) =
        let !days = getValue oct (toByteOffset off) BiE
            !milli = getValue oct (toByteOffset off + 2) BiE
            !micro = getValue oct (toByteOffset off + 6) BiE
            !encTime = mkCDSTime days milli (Just micro)
            st = cdsTimeToSunTime epoch encTime
        in
        TMValTime st
    readTime oct off epoch (CDS6 delta) =
        let !days = getValue oct (toByteOffset off) BiE
            !milli = getValue oct (toByteOffset off + 2) BiE
            !encTime = mkCDSTime days milli Nothing
            st = cdsTimeToSunTime epoch encTime
        in
        TMValTime st

