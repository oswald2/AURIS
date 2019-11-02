{-# LANGUAGE TypeApplications #-}
module Data.PUS.TMPacketProcessing
    ( packetProcessorC
    , raiseTMPacketC
    , getPackeDefinition
    )
where


import           RIO
import qualified RIO.ByteString                as B
import qualified RIO.Vector                    as V
import           RIO.List                      ( iterate )
import           Conduit
import           Control.Lens                  ( (.~) )
import           Data.HashTable.ST.Basic       as HT
import qualified Data.Text.Short               as ST
import qualified VectorBuilder.Builder         as VB
import qualified VectorBuilder.Vector          as VB

import           Control.PUS.Classes
import           Data.DataModel

import           Data.TM.TMPacketDef
import           Data.TM.TMParameterDef
import           Data.TM.PIVals
import           Data.TM.Parameter
import           Data.TM.Value
import           Data.TM.Validity

import           Data.PUS.Config
import           Data.PUS.ExtractedDU
import           Data.PUS.PUSPacket
import           Data.PUS.PUSDfh
import           Data.PUS.TMPacket
--import           Data.PUS.MissionSpecific.Definitions
import           Data.PUS.EncTime
import           Data.PUS.PUSState
import           Data.PUS.CRC
import           Data.PUS.Events

import           General.GetBitField
import           General.Types
import           General.Time



raiseTMPacketC :: (MonadIO m, MonadReader env m, HasGlobalState env) 
  => ConduitT TMPacket TMPacket m () 
raiseTMPacketC = awaitForever $ \pkt -> do 
  env <- ask 
  liftIO $ raiseEvent env (EVTelemetry (EVTMPacketDecoded pkt))
  yield pkt 


packetProcessorC
    :: ( MonadIO m
       , MonadReader env m
       , HasDataModel env
       , HasCorrelationState env
       , HasPUSState env
       , HasLogFunc env
       , HasConfig env
       )
    => ConduitT (ByteString, ExtractedDU PUSPacket) TMPacket m ()
packetProcessorC = awaitForever $ \pkt@(oct, pusPkt) -> do
    model' <- view getDataModel
    model  <- readTVarIO model'
    cfg <- view getConfig
    let def' = getPackeDefinition model pkt
    case def' of
        Just def -> do 
          if def ^. tmpdCheck 
            then do 
              case crcCheck oct of 
                Left err -> logError $ "CRC Check on TM packet failed: " 
                      <> display err <> ". Packet ignored."
                Right (chk, payload, crcCalcd, crcPkt) -> do 
                  if chk 
                    then yieldM $ processPacket def (payload, pusPkt) 
                    else logError $ "CRC Check on TM packet failed: received: " 
                            <> display crcPkt 
                            <> ", calculated: " 
                            <> display crcCalcd
                            <> ". Packet ignored."
            else yieldM $ processPacket def pkt
        Nothing  -> do
            -- if we have a packet, that is not found in the data model,
            -- we create a new TM packet from it where the data part 
            -- is the data part of the PUS packet
            logWarn
                $  "No packet defintion found for packet: APID:"
                <> display (pusPkt ^. epDU . pusHdr . pusHdrTcApid)
                <> " Type:"
                <> display (pusType (pusPkt ^. epDU . pusDfh))
                <> " SubType:"
                <> display (pusSubType (pusPkt ^. epDU . pusDfh))
            (timeStamp, _epoch) <- getTimeStamp pkt 

            let param = TMParameter {
                  _pName = "Content"
                  , _pTime = timeStamp
                  , _pValue = TMValue (TMValOctet (pusPkt ^. epDU . pusData)) clearValidity
                  , _pEngValue = Nothing
                  }

                tmpkt = TMPacket {
                    _tmpSPID = cfgUnknownSPID cfg
                    , _tmpMnemonic = "UNKNOWN PACKET"
                    , _tmpDescr = ""
                    , _tmpAPID = pusPkt ^. epDU . pusHdr . pusHdrTcApid
                    , _tmpType = pusType (pusPkt ^. epDU . pusDfh)
                    , _tmpSubType = pusSubType (pusPkt ^. epDU . pusDfh)
                    , _tmpERT = pusPkt ^. epERT
                    , _tmpTimeStamp = timeStamp
                    , _tmpVCID = pusPkt ^. epVCID 
                    , _tmpSSC = pusPkt ^. epDU . pusHdr . pusHdrTcSsc
                    , _tmpParams = V.singleton param
                    }

            yield tmpkt 



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
processPacket pktDef pkt@(_, pusDU) = do
    (timestamp, epoch) <- getTimeStamp pkt
    params <- getParameters timestamp epoch pktDef pkt 
    let tmPacket = TMPacket {
      _tmpSPID = _tmpdSPID pktDef
      , _tmpMnemonic = _tmpdName pktDef
      , _tmpDescr = _tmpdDescr pktDef
      , _tmpAPID = _tmpdApid pktDef 
      , _tmpType = _tmpdType pktDef 
      , _tmpSubType = _tmpdSubType pktDef
      , _tmpERT = pusDU ^. epERT
      , _tmpTimeStamp = timestamp 
      , _tmpVCID = pusDU ^. epVCID
      , _tmpSSC = pusDU ^. epDU . pusHdr . pusHdrTcSsc
      , _tmpParams = params
      }
    return tmPacket


getTimeStamp
    :: (MonadIO m, MonadReader env m, HasPUSState env, HasCorrelationState env)
    => (ByteString, ExtractedDU PUSPacket)
    -> m (SunTime, Epoch)
getTimeStamp (_, epu) = do
    appState <- view appStateG
    state    <- readTVarIO appState

    let epoch = state ^. pusStEpoch

    case pusPktTime (epu ^. epDU . pusDfh) of
        Just time -> do
            let sunTime = cucTimeToSunTime epoch time
            t <- correlateTMTime sunTime (epu ^. epERT)
            return (t, epoch)
        Nothing -> do
            case state ^. pusStCorrelation of
                CorrERT -> return (epu ^. epERT, epoch)
                _       -> do
                  t <- liftIO getCurrentTime
                  return (t, epoch)



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


getParameters :: (MonadIO m, MonadReader env m, HasPUSState env, HasCorrelationState env) 
  => SunTime 
  -> Epoch 
  -> TMPacketDef 
  -> (ByteString, ExtractedDU PUSPacket) 
  -> m (Vector TMParameter)
getParameters timestamp epoch def pkt = do
  let ert = snd pkt ^. epERT
  case def ^. tmpdParams of 
    TMFixedParams paramLocations -> getFixedParams timestamp ert epoch def pkt paramLocations 
    TMVariableParams tpsd dfhSize -> getVariableParams timestamp def pkt tpsd dfhSize


getFixedParams :: (MonadIO m, MonadReader env m, HasPUSState env, HasCorrelationState env) 
  => SunTime 
  -> SunTime
  -> Epoch 
  -> TMPacketDef 
  -> (ByteString, ExtractedDU PUSPacket) 
  -> Vector TMParamLocation 
  -> m (Vector TMParameter) 
getFixedParams timestamp ert epoch _def (oct', _ep) locs = do 
  bldr <- V.foldM (go oct') VB.empty locs
  return (VB.build bldr)  
  where 
    go oct bldr loc = do 
      case _tmplSuperComm loc of 
        Just sup -> do 
          let newLocs = unroll loc sup 
              func (l, ts) = do 
                v <- getParamValue epoch ert oct l 
                return TMParameter {
                    _pName = _tmplName loc 
                    , _pTime = ts 
                    , _pValue = v 
                    , _pEngValue = Nothing
                  }
          vals <- mapM func newLocs 
          return (bldr <> VB.foldable vals) 
        Nothing -> do 
          val <- getParamValue epoch ert oct loc 

          let parTime = if tOffset == nullRelTime then timestamp else timestamp <+> tOffset
              tOffset = _tmplTime loc

          let paramValue = TMParameter {
                _pName = _tmplName loc  
                , _pTime = parTime 
                , _pValue = val 
                , _pEngValue = Nothing
              }
          return (bldr <> VB.singleton paramValue)
          
    unroll :: TMParamLocation -> SuperCommutated -> [(TMParamLocation, SunTime)]
    unroll loc sup = 
      let n = sup ^. scNbOcc in 
      if n <= 1 
        then [(loc, timestamp <+> (loc ^. tmplTime))] 
        else
          let newLocs = update sup . replicate (sup ^. scNbOcc) $ loc
              startTime = timestamp <+> (loc ^. tmplTime)
              times = iterate f startTime
              deltaTime = sup ^. scTdOcc
              f x = addSpan x deltaTime 
          in zip newLocs times  


    update :: SuperCommutated 
      -> [TMParamLocation] 
      -> [TMParamLocation] 
    update _ [] = []
    update _ [locNew] = [locNew] 
    update sup (locOld : locNew : rest) =
      let oldOff = locOld ^. tmplOffset 
          oldWidth = getWidth (locOld ^. tmplParam)
          newOff = oldOff .+. oldWidth .+. sup ^. scLgOcc
          locNew' = locNew & tmplOffset .~ newOff 
      in 
      locOld : update sup (locNew' : rest) 




getVariableParams :: 
  SunTime 
  -> TMPacketDef 
  -> (ByteString, ExtractedDU PUSPacket) 
  -> Int 
  -> Word8 
  -> m (Vector TMParameter)
getVariableParams = undefined 


getParamValue :: (MonadIO m, MonadReader env m, HasPUSState env, HasCorrelationState env) 
  => Epoch -> SunTime -> ByteString -> TMParamLocation -> m TMValue
getParamValue epoch ert oct def = do 
  let (val, corr) = extractParamValue epoch oct def 
  if corr == CorrelationYes
    then do 
      case val ^. tmvalValue of 
          TMValTime inTime -> do
            newTime <- TMValTime <$> correlateTMTime inTime ert
            return $ val & tmvalValue .~ newTime
          _ -> return val
    else return val 



extractParamValue :: Epoch -> ByteString -> TMParamLocation -> (TMValue, Correlate)
extractParamValue epoch oct def =
    let
        par    = def ^. tmplParam
        endian = par ^. fpEndian
    in
        case def ^. tmplParam . fpType of
            ParamInteger w ->
                let v = readIntParam (_tmplOffset def) (BitSize w) endian
                in  (TMValue v clearValidity, CorrelationNo)
            ParamUInteger w ->
                let v = readUIntParam (_tmplOffset def) (BitSize w) endian
                in  (TMValue v clearValidity, CorrelationNo)
            ParamDouble dt ->
                let v = readDoubleParam (_tmplOffset def) endian dt
                in  (TMValue v clearValidity, CorrelationNo)
            ParamString w ->
                let v = readString (_tmplOffset def) w
                in  (TMValue v clearValidity, CorrelationNo)
            ParamOctet w ->
                let v = readOctet (_tmplOffset def) w
                in (TMValue v clearValidity, CorrelationNo)
            ParamTime tt ct ->
                let v = readTime (_tmplOffset def) tt
                in (TMValue v clearValidity, ct)
            ParamDeduced _ -> 
                -- TODO handle deduced params
                (TMValue TMValNothing clearValidity, CorrelationNo)
            ParamSavedSynthetic -> 
                -- TODO handle synthetic params
                (TMValue TMValNothing clearValidity, CorrelationNo)
            
            
  where
    readIntParam off width endian = if isByteAligned off
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
                (getBitFieldInt64 oct (_tmplOffset def) width endian)
    readUIntParam off width endian = if isByteAligned off
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


    readDoubleParam off endian DTDouble = if isByteAligned off
        then TMValDouble (getValue @Double oct (toByteOffset off) endian)
        else TMValDouble (getBitFieldDouble oct (toOffset off) endian)
    readDoubleParam off endian DTFloat = if isByteAligned off
        then TMValDouble
            (realToFrac (getValue @Float oct (toByteOffset off) endian))
        else TMValDouble
            (realToFrac (getBitFieldFloat oct (toOffset off) endian))
    readDoubleParam off endian DTMilSingle = if isByteAligned off
        then
            TMValDouble
                (getMilSingle
                    (getValue @MILSingle oct (toByteOffset off) endian)
                )
        else TMValDouble (getBitFieldMilSingle oct (toOffset off) endian)
    readDoubleParam off endian DTMilExtended = if isByteAligned off
        then TMValDouble
            (getMilExtended
                (getValue @MILExtended oct (toByteOffset off) endian)
            )
        else TMValDouble (getBitFieldMilExtended oct (toOffset off) endian)

    readString off (Just w) =
        let val = B.take w $ B.drop (unByteOffset (toByteOffset off)) oct
        in  case ST.fromByteString val of
                Just x  -> TMValString x
                Nothing -> TMValString "UNDEFINED"
    readString off Nothing =
        let val = B.drop (unByteOffset (toByteOffset off)) oct
        in  case ST.fromByteString val of
                Just x  -> TMValString x
                Nothing -> TMValString "UNDEFINED"

    readOctet off (Just w) =
        let val = B.take w $ B.drop (unByteOffset (toByteOffset off)) oct
        in TMValOctet val
    readOctet off Nothing =
        let val = B.drop (unByteOffset (toByteOffset off)) oct
        in TMValOctet val

    readTime off (CUC4Coarse2Fine delta) =
        let !sec = getValue @Int32 oct (toByteOffset off) BiE
            !mic = getValue @Int16 oct (toByteOffset off + 4) BiE
            !encTime = mkCUCTime (fromIntegral sec) (fromIntegral mic) delta
            st = cucTimeToSunTime epoch encTime
        in
        TMValTime st
    readTime off (CDS8 _) =
        let !days = getValue oct (toByteOffset off) BiE
            !milli = getValue oct (toByteOffset off + 2) BiE
            !micro = getValue oct (toByteOffset off + 6) BiE
            !encTime = mkCDSTime days milli (Just micro)
            st = cdsTimeToSunTime epoch encTime
        in
        TMValTime st
    readTime off (CDS6 _) =
        let !days = getValue oct (toByteOffset off) BiE
            !milli = getValue oct (toByteOffset off + 2) BiE
            !encTime = mkCDSTime days milli Nothing
            st = cdsTimeToSunTime epoch encTime
        in
        TMValTime st

