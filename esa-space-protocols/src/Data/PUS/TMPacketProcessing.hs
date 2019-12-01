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
import qualified RIO.Text                      as T
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

--import           General.Hexdump



raiseTMPacketC :: (MonadIO m, MonadReader env m, HasGlobalState env)
  => ConduitT TMPacket TMPacket m ()
raiseTMPacketC = awaitForever $ \pkt -> do
  env <- ask
  liftIO $ raiseEvent env (EVTelemetry (EVTMPacketDecoded pkt))
  yield pkt


packetProcessorC
    :: ( MonadIO m
       , MonadReader env m
       , HasGlobalState env
       )
    => ConduitT (ByteString, ExtractedDU PUSPacket) TMPacket m ()
packetProcessorC = awaitForever $ \pkt@(oct, pusPkt) -> do

    logDebug $ display ("packetProcessorC: got packet: " :: Text) <> displayShow pkt

    model' <- view getDataModel
    model  <- readTVarIO model'
    cfg <- view getConfig
    let def' = getPackeDefinition model pkt
    case def' of
        Just (key, def) -> do
          if def ^. tmpdCheck
            then do
              case crcCheck oct of
                Left err -> logError $ "CRC Check on TM packet failed: "
                      <> display err <> ". Packet ignored."
                Right (chk, payload, crcReceived, crcCalcd) -> do
                  if chk
                    then yieldM $ processPacket def key (payload, pusPkt)
                    else logError $ "CRC Check on TM packet failed: received: "
                            <> display crcReceived
                            <> ", calculated: "
                            <> display crcCalcd
                            <> ". Packet ignored."
            else yieldM $ processPacket def key pkt
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
                    , _tmpPI1 = 0
                    , _tmpPI2 = 0
                    , _tmpERT = pusPkt ^. epERT
                    , _tmpTimeStamp = timeStamp
                    , _tmpVCID = pusPkt ^. epVCID
                    , _tmpSSC = pusPkt ^. epDU . pusHdr . pusHdrTcSsc
                    , _tmpEvent = PIDNo
                    , _tmpParams = V.singleton param
                    }

            yield tmpkt



-- | Main function to detect the packet identification of a newly received
-- TM PUS Packet.
getPackeDefinition
    :: DataModel -> (ByteString, ExtractedDU PUSPacket) -> Maybe (TMPacketKey, TMPacketDef)
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
    in
      case HT.ilookup (model ^. dmTMPackets) pktKey of
        Nothing -> Nothing
        Just p -> Just (pktKey, p)





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
    :: (MonadIO m, MonadReader env m, HasGlobalState env)
    => TMPacketDef
    -> TMPacketKey
    -> (ByteString, ExtractedDU PUSPacket)
    -> m TMPacket
processPacket pktDef (TMPacketKey _apid _t _st pi1 pi2) pkt@(_, pusDU) = do
    (timestamp, epoch) <- getTimeStamp pkt

    logDebug $ display ("TimeStamp: " :: Text) <> display timestamp <> display (". Getting parameters..." :: Text)

    params <- getParameters timestamp epoch pktDef pkt

    logDebug $ display ("Got Parameters: " :: Text) <> displayShow params

    let tmPacket = TMPacket {
      _tmpSPID = _tmpdSPID pktDef
      , _tmpMnemonic = _tmpdName pktDef
      , _tmpDescr = _tmpdDescr pktDef
      , _tmpAPID = _tmpdApid pktDef
      , _tmpType = _tmpdType pktDef
      , _tmpSubType = _tmpdSubType pktDef
      , _tmpPI1 = fromIntegral pi1
      , _tmpPI2 = fromIntegral pi2
      , _tmpERT = pusDU ^. epERT
      , _tmpTimeStamp = timestamp
      , _tmpVCID = pusDU ^. epVCID
      , _tmpSSC = pusDU ^. epDU . pusHdr . pusHdrTcSsc
      , _tmpEvent = _tmpdEvent pktDef
      , _tmpParams = params
      }

    -- check for an event
    case _tmpdEvent pktDef of
      PIDNo -> return ()
      PIDInfo txt -> do
        env <- ask
        let msg = utf8BuilderToText $ display ("SPID: " :: Text)
               <> display (_tmpdSPID pktDef)
               <> " APID: " <> display (_tmpdApid pktDef)
               <> " Type: " <> display (_tmpdType pktDef)
               <> " SubType: " <> display (_tmpdSubType pktDef)
               <> " SSC: " <> display (_tmpSSC tmPacket)
               <> " Msg: " <> display (ST.toText txt)
        liftIO $ raiseEvent env (EVAlarms (EVPacketInfo msg))
      PIDWarning txt -> do
        env <- ask
        let msg = utf8BuilderToText $ display ("SPID: " :: Text)
               <> display (_tmpdSPID pktDef)
               <> " APID: " <> display (_tmpdApid pktDef)
               <> " Type: " <> display (_tmpdType pktDef)
               <> " SubType: " <> display (_tmpdSubType pktDef)
               <> " SSC: " <> display (_tmpSSC tmPacket)
               <> " Msg: " <> display (ST.toText txt)
        liftIO $ raiseEvent env (EVAlarms (EVPacketWarn msg))
      PIDAlarm txt -> do
        env <- ask
        let msg = utf8BuilderToText $ display ("SPID: " :: Text)
               <> display (_tmpdSPID pktDef)
               <> " APID: " <> display (_tmpdApid pktDef)
               <> " Type: " <> display (_tmpdType pktDef)
               <> " SubType: " <> display (_tmpdSubType pktDef)
               <> " SSC: " <> display (_tmpSSC tmPacket)
               <> " Msg: " <> display (ST.toText txt)
        liftIO $ raiseEvent env (EVAlarms (EVPacketAlarm msg))
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


getParameters :: (MonadIO m, MonadReader env m, HasPUSState env, 
    HasCorrelationState env, HasDataModel env)
  => SunTime
  -> Epoch
  -> TMPacketDef
  -> (ByteString, ExtractedDU PUSPacket)
  -> m (Vector TMParameter)
getParameters timestamp epoch def pkt = do
  let ert = snd pkt ^. epERT
  case def ^. tmpdParams of
    TMFixedParams paramLocations -> getFixedParams timestamp ert epoch def pkt paramLocations
    TMVariableParams tpsd dfhSize varParams -> getVariableParams timestamp ert epoch def pkt tpsd dfhSize varParams


getFixedParams :: (MonadIO m, MonadReader env m, HasPUSState env, HasCorrelationState env)
  => SunTime
  -> SunTime
  -> Epoch
  -> TMPacketDef
  -> (ByteString, ExtractedDU PUSPacket)
  -> Vector TMParamLocation
  -> m (Vector TMParameter)
getFixedParams timestamp ert epoch _def (oct', _ep) locs = do
  VB.build <$> V.foldM (go oct') VB.empty locs
  where
    go oct bldr loc = do
      case _tmplSuperComm loc of
        Just sup -> do
          let newLocs = unroll loc sup
              func (l, ts) = do
                v <- getParamValue epoch ert oct (l ^. tmplOffset) (l ^. tmplParam)
                return TMParameter {
                    _pName = _tmplName loc
                    , _pTime = ts
                    , _pValue = v
                    , _pEngValue = Nothing
                  }
          vals <- mapM func newLocs
          return (bldr <> VB.foldable vals)
        Nothing -> do
          val <- getParamValue epoch ert oct (loc ^. tmplOffset) (loc ^. tmplParam)

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




getVariableParams :: (MonadIO m, MonadReader env m, 
    HasPUSState env, HasCorrelationState env, HasDataModel env) => 
  SunTime
  -> SunTime 
  -> Epoch 
  -> TMPacketDef
  -> (ByteString, ExtractedDU PUSPacket)
  -> Int
  -> Word8
  -> VarParams 
  -> m (Vector TMParameter)
getVariableParams timestamp ert epoch pktDef (oct', ep) _tpsd dfhSize varParams = do 
    let offset = mkOffset (fromIntegral dfhSize * 8) 0
    params <- go offset varParams 
    return (VB.build (snd params))
    where 
        go !offset VarParamsEmpty = return (offset, VB.empty)
        -- Normal parameter, just extract the value, recurse for the rest 
        go !offset (VarNormal vpdParDef rest) = do 
            let newOffset = offset .+. getPaddedWidth parDef .+. vpdParDef ^. tmvpOffset
                extractOffset = offset .+. getPadding parDef 
                parDef = vpdParDef ^. tmvpParam
            val <- getParamValue epoch ert oct' extractOffset parDef
            
            let !param = TMParameter {
                    _pName = vpdParDef ^. tmvpName
                    , _pTime = timestamp 
                    , _pValue = val 
                    , _pEngValue = Nothing
                    }
            
            (lastOff, next) <- go newOffset rest 

            return (lastOff, VB.singleton param <> next)

        -- Group repeater. 
        go !offset (VarGroup repeater group rest) = do 
            let newOffset = offset .+. getPaddedWidth parDef  .+. repeater ^. tmvpOffset
                extractOffset = offset .+. getPadding parDef 
                parDef = repeater ^. tmvpParam 
            -- The value specifies, how often the group is repeated
            nval <- getParamValue epoch ert oct' extractOffset parDef 
            (lastOff, parVals) <- case nval of 
                TMValue (TMValInt x) _ -> processGroup x newOffset group 
                TMValue (TMValUInt x) _ -> processGroup (fromIntegral x) newOffset group 
                TMValue (TMValDouble x) _ -> processGroup (truncate x) newOffset group 
                _ -> go newOffset rest 
            
            let !param = TMParameter {
                _pName = repeater ^. tmvpName
                , _pTime = timestamp 
                , _pValue = nval 
                , _pEngValue = Nothing
                }

            -- now take the rest of the packet 
            (lastOff2, parVals2) <- go lastOff rest 
            return (lastOff2, VB.singleton param <> parVals <> parVals2)

        -- Fixed repeater. 
        go !offset (VarFixed reps group rest) = do 
            -- reps specifies, how often the group is repeated
            (lastOff, parVals) <- processGroup (fromIntegral reps) offset group 

            -- now take the rest of the packet 
            (lastOff2, parVals2) <- go lastOff rest 
            return (lastOff2, parVals <> parVals2)

        go !offset (VarChoice par) = do 
            let !newOffset = offset .+. getPaddedWidth parDef  .+. par ^. tmvpOffset
                !extractOffset = offset .+. getPadding parDef 
                parDef = par ^. tmvpParam 
            -- The value specifies, how often the group is repeated
            tpsdval <- getParamValue epoch ert oct' extractOffset parDef
            
            let !param = TMParameter {
                _pName = par ^. tmvpName
                , _pTime = timestamp 
                , _pValue = tpsdval 
                , _pEngValue = Nothing
                }

            choiceRes <- case tpsdval of 
                TMValue (TMValInt x) _ -> processChoice x newOffset  
                TMValue (TMValUInt x) _ -> processChoice (fromIntegral x) newOffset
                TMValue (TMValDouble x) _ -> processChoice (truncate x) newOffset
                _ -> return $ Left $ utf8BuilderToText $ 
                    display @Text "CHOICE parameter " 
                    <> display (par ^. tmvpName)
                    <> display @Text ": illegal data type " 
                    <> displayShow (parDef ^. fpType)
                    <> display @Text " in packet " 
                    <> display (pktDef ^. tmpdSPID)
                    <> display @Text " ("
                    <> display (pktDef ^. tmpdName)
                    <> display @Text ") APID: "
                    <> display (pktDef ^. tmpdApid)
                    <> display @Text " Type: "
                    <> display (pktDef ^. tmpdType)
                    <> display @Text " SubType: "
                    <> display (pktDef ^. tmpdSubType)
                    <> display @Text " APID: "
                    <> display (ep ^. epDU . pusHdr . pusHdrTcSsc)

            case choiceRes of 
                Left err -> do undefined 
                Right (lastOffset, params) -> do 
                    return (lastOffset, VB.singleton param <> params)

        -- TODO
        go !offset (VarPidRef _ _) = do
            traceM "CHOICE parameters not yet implemented"
            return (offset, VB.empty)


        processGroup 0 offset _ = return (offset, VB.empty)
        processGroup !n offset params = do 
            (newOff, parVals) <- go offset params 
            (lastOff, nextVals) <- processGroup (n - 1) newOff params 
            return (lastOff, parVals <> nextVals)

        processChoice !tpsd !offset = do 
            env <- ask 
            dm <- readTVarIO (env ^. getDataModel)
            let vpds = dm ^. dmVPDStructs
            case HT.ilookup vpds (fromIntegral tpsd) of 
                Nothing -> return $ Left ("Choice parameter: could not find TPSD " <> T.pack (show tpsd) <> " in VPD")
                Just struct -> Right <$> go offset struct 


getParamValue :: (MonadIO m, MonadReader env m, HasPUSState env, HasCorrelationState env)
  => Epoch -> SunTime -> ByteString -> Offset -> TMParameterDef -> m TMValue
getParamValue epoch ert oct off parDef = do
  let (val, corr) = extractParamValue epoch oct off parDef 
  if corr == CorrelationYes
    then do
      case val ^. tmvalValue of
          TMValTime inTime -> do
            newTime <- TMValTime <$> correlateTMTime inTime ert
            return $ val & tmvalValue .~ newTime
          _ -> return val
    else return val



extractParamValue :: 
    Epoch 
    -> ByteString 
    -> Offset 
    -> TMParameterDef 
    -> (TMValue, Correlate)
extractParamValue epoch oct offset par =
    let endian = par ^. fpEndian
    in
        case par ^. fpType of
            ParamInteger w ->
                let v = readIntParam offset (BitSize w) endian
                in  (TMValue v clearValidity, CorrelationNo)
            ParamUInteger w ->
                let v = readUIntParam offset (BitSize w) endian
                in  (TMValue v clearValidity, CorrelationNo)
            ParamDouble dt ->
                let v = readDoubleParam offset endian dt
                in  (TMValue v clearValidity, CorrelationNo)
            ParamString w ->
                let v = readString offset w
                in  (TMValue v clearValidity, CorrelationNo)
            ParamOctet w ->
                let v = readOctet offset w
                in (TMValue v clearValidity, CorrelationNo)
            ParamTime tt ct ->
                let v = readTime offset tt
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
            w  -> TMValInt (getBitFieldInt64 oct offset w endian)
        else
            TMValInt
                (getBitFieldInt64 oct offset width endian)
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
            w  -> TMValUInt (getBitFieldWord64 oct offset w endian)
        else TMValUInt (getBitFieldWord64 oct offset width endian)


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

    readTime off (CUC1 False) =
      let !sec = getValue @Word8 oct (toByteOffset off) BiE
          !encTime = mkCUC sec False
      in TMValTime (cucTimeToSunTime epoch encTime)
    readTime off (CUC1 True) =
      let !sec = getValue @Int8 oct (toByteOffset off) BiE
          !encTime = mkCUC sec True
      in TMValTime (cucTimeToSunTime epoch encTime)

    readTime off (CUC1_1 False) =
      let !sec = getValue @Word8 oct (toByteOffset off) BiE
          !mic = getValue @Word8 oct (toByteOffset off + 1) BiE
          !encTime = mkCUC_1 sec mic False
      in TMValTime (cucTimeToSunTime epoch encTime)
    readTime off (CUC1_1 True) =
      let !sec = getValue @Int8 oct (toByteOffset off) BiE
          !mic = getValue @Word8 oct (toByteOffset off + 1) BiE
          !encTime = mkCUC_1 sec mic True
      in TMValTime (cucTimeToSunTime epoch encTime)

    readTime off (CUC1_2 False) =
      let !sec = getValue @Word8 oct (toByteOffset off) BiE
          !mic = getValue @Word16 oct (toByteOffset off + 1) BiE
          !encTime = mkCUC_2 sec mic False
      in TMValTime (cucTimeToSunTime epoch encTime)
    readTime off (CUC1_2 True) =
      let !sec = getValue @Int8 oct (toByteOffset off) BiE
          !mic = getValue @Word16 oct (toByteOffset off + 1) BiE
          !encTime = mkCUC_2 sec mic True
      in TMValTime (cucTimeToSunTime epoch encTime)

    readTime off (CUC1_3 False) =
        let !sec = getValue @Word8 oct (toByteOffset off) BiE
            !mic = getValue @Word24 oct (toByteOffset off + 1) BiE
            !encTime = mkCUC_3 sec mic False
        in TMValTime (cucTimeToSunTime epoch encTime)
    readTime off (CUC1_3 True) =
        let !sec = getValue @Int8 oct (toByteOffset off) BiE
            !mic = getValue @Word24 oct (toByteOffset off + 1) BiE
            !encTime = mkCUC_3 sec mic True
        in TMValTime (cucTimeToSunTime epoch encTime)

    readTime off (CUC2 False) =
        let !sec = getValue @Word16 oct (toByteOffset off) BiE
            !encTime = mkCUC sec False
        in TMValTime (cucTimeToSunTime epoch encTime)
    readTime off (CUC2 True) =
        let !sec = getValue @Int16 oct (toByteOffset off) BiE
            !encTime = mkCUC sec True
        in TMValTime (cucTimeToSunTime epoch encTime)

    readTime off (CUC2_1 False) =
      let !sec = getValue @Word16 oct (toByteOffset off) BiE
          !mic = getValue @Word8 oct (toByteOffset off + 1) BiE
          !encTime = mkCUC_1 sec mic False
      in TMValTime (cucTimeToSunTime epoch encTime)
    readTime off (CUC2_1 True) =
      let !sec = getValue @Int16 oct (toByteOffset off) BiE
          !mic = getValue @Word8 oct (toByteOffset off + 1) BiE
          !encTime = mkCUC_1 sec mic True
      in TMValTime (cucTimeToSunTime epoch encTime)

    readTime off (CUC2_2 False) =
      let !sec = getValue @Word16 oct (toByteOffset off) BiE
          !mic = getValue @Word16 oct (toByteOffset off + 1) BiE
          !encTime = mkCUC_2 sec mic False
      in TMValTime (cucTimeToSunTime epoch encTime)
    readTime off (CUC2_2 True) =
      let !sec = getValue @Int16 oct (toByteOffset off) BiE
          !mic = getValue @Word16 oct (toByteOffset off + 1) BiE
          !encTime = mkCUC_2 sec mic True
      in TMValTime (cucTimeToSunTime epoch encTime)

    readTime off (CUC2_3 False) =
        let !sec = getValue @Word16 oct (toByteOffset off) BiE
            !mic = getValue @Word24 oct (toByteOffset off + 1) BiE
            !encTime = mkCUC_3 sec mic False
        in TMValTime (cucTimeToSunTime epoch encTime)
    readTime off (CUC2_3 True) =
        let !sec = getValue @Int16 oct (toByteOffset off) BiE
            !mic = getValue @Word24 oct (toByteOffset off + 1) BiE
            !encTime = mkCUC_3 sec mic True
        in TMValTime (cucTimeToSunTime epoch encTime)
    readTime off (CUC3 False) =
        let !sec = getValue @Word24 oct (toByteOffset off) BiE
            !encTime = mkCUC sec False
        in TMValTime (cucTimeToSunTime epoch encTime)
    readTime off (CUC3 True) =
        let !sec = getValue @Int24 oct (toByteOffset off) BiE
            !encTime = mkCUC sec True
        in TMValTime (cucTimeToSunTime epoch encTime)

    readTime off (CUC3_1 False) =
      let !sec = getValue @Word24 oct (toByteOffset off) BiE
          !mic = getValue @Word8 oct (toByteOffset off + 1) BiE
          !encTime = mkCUC_1 sec mic False
      in TMValTime (cucTimeToSunTime epoch encTime)
    readTime off (CUC3_1 True) =
      let !sec = getValue @Int24 oct (toByteOffset off) BiE
          !mic = getValue @Word8 oct (toByteOffset off + 1) BiE
          !encTime = mkCUC_1 sec mic True
      in TMValTime (cucTimeToSunTime epoch encTime)

    readTime off (CUC3_2 False) =
      let !sec = getValue @Word24 oct (toByteOffset off) BiE
          !mic = getValue @Word16 oct (toByteOffset off + 1) BiE
          !encTime = mkCUC_2 sec mic False
      in TMValTime (cucTimeToSunTime epoch encTime)
    readTime off (CUC3_2 True) =
      let !sec = getValue @Int24 oct (toByteOffset off) BiE
          !mic = getValue @Word16 oct (toByteOffset off + 1) BiE
          !encTime = mkCUC_2 sec mic True
      in TMValTime (cucTimeToSunTime epoch encTime)

    readTime off (CUC3_3 False) =
        let !sec = getValue @Word24 oct (toByteOffset off) BiE
            !mic = getValue @Word24 oct (toByteOffset off + 1) BiE
            !encTime = mkCUC_3 sec mic False
        in TMValTime (cucTimeToSunTime epoch encTime)
    readTime off (CUC3_3 True) =
        let !sec = getValue @Int24 oct (toByteOffset off) BiE
            !mic = getValue @Word24 oct (toByteOffset off + 1) BiE
            !encTime = mkCUC_3 sec mic True
        in TMValTime (cucTimeToSunTime epoch encTime)


    readTime off (CUC4 False) =
        let !sec = getValue @Word32 oct (toByteOffset off) BiE
            !encTime = mkCUC sec False
        in TMValTime (cucTimeToSunTime epoch encTime)
    readTime off (CUC4 True) =
        let !sec = getValue @Int32 oct (toByteOffset off) BiE
            !encTime = mkCUC sec True
        in TMValTime (cucTimeToSunTime epoch encTime)

    readTime off (CUC4_1 False) =
      let !sec = getValue @Word32 oct (toByteOffset off) BiE
          !mic = getValue @Word8 oct (toByteOffset off + 1) BiE
          !encTime = mkCUC_1 sec mic False
      in TMValTime (cucTimeToSunTime epoch encTime)
    readTime off (CUC4_1 True) =
      let !sec = getValue @Int32 oct (toByteOffset off) BiE
          !mic = getValue @Word8 oct (toByteOffset off + 1) BiE
          !encTime = mkCUC_1 sec mic True
      in TMValTime (cucTimeToSunTime epoch encTime)

    readTime off (CUC4_2 False) =
      let !sec = getValue @Word32 oct (toByteOffset off) BiE
          !mic = getValue @Word16 oct (toByteOffset off + 1) BiE
          !encTime = mkCUC_2 sec mic False
      in TMValTime (cucTimeToSunTime epoch encTime)
    readTime off (CUC4_2 True) =
      let !sec = getValue @Int32 oct (toByteOffset off) BiE
          !mic = getValue @Word16 oct (toByteOffset off + 1) BiE
          !encTime = mkCUC_2 sec mic True
      in TMValTime (cucTimeToSunTime epoch encTime)

    readTime off (CUC4_3 False) =
        let !sec = getValue @Word32 oct (toByteOffset off) BiE
            !mic = getValue @Word24 oct (toByteOffset off + 1) BiE
            !encTime = mkCUC_3 sec mic False
        in TMValTime (cucTimeToSunTime epoch encTime)
    readTime off (CUC4_3 True) =
        let !sec = getValue @Int32 oct (toByteOffset off) BiE
            !mic = getValue @Word24 oct (toByteOffset off + 1) BiE
            !encTime = mkCUC_3 sec mic True
        in TMValTime (cucTimeToSunTime epoch encTime)

    readTime off UxTime =
        let !sec = getValue @Word32 oct (toByteOffset off) BiE
            !mic = getValue @Word32 oct (toByteOffset off + 1) BiE
            !encTime = mkCUCTime (fromIntegral sec) (fromIntegral mic) False
        in TMValTime (cucTimeToSunTime epoch encTime)

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

