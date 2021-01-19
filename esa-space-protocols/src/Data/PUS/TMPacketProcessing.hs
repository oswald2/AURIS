{-# LANGUAGE TypeApplications #-}
module Data.PUS.TMPacketProcessing
    ( packetProcessorC
    , raiseTMPacketC
    , raiseTMParameterC
    , getPackeDefinition
    ) where


import           RIO                     hiding ( (.~) )
import qualified RIO.ByteString                as B
import qualified RIO.Vector                    as V
import qualified RIO.Text                      as T
import           RIO.List                       ( iterate )
import           Conduit
import           Control.Lens                   ( (.~) )
import           Data.HashTable.ST.Basic       as HT
import qualified Data.Text.Short               as ST
import qualified VectorBuilder.Builder         as VB
import qualified VectorBuilder.Vector          as VB

import qualified Data.Attoparsec.ByteString    as A
import qualified Data.Attoparsec.Binary        as A

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
import           Data.PUS.ExtractedPUSPacket
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
import           General.Hexdump
import           General.PUSTypes

import           Verification.Verification



raiseTMPacketC
    :: (MonadIO m, MonadReader env m, HasGlobalState env)
    => ConduitT TMPacket TMPacket m ()
raiseTMPacketC = awaitForever $ \pkt -> do
    env <- ask
    liftIO $ raiseEvent env (EVTelemetry (EVTMPacketDecoded pkt))
    yield pkt


raiseTMParameterC
    :: (MonadIO m, MonadReader env m, HasGlobalState env)
    => ConduitT TMPacket TMPacket m ()
raiseTMParameterC = awaitForever $ \pkt -> do
    env <- ask
    liftIO $ raiseEvent env (EVTelemetry (EVTMParameters (pkt ^. tmpParams)))


packetProcessorC
    :: (MonadIO m, MonadReader env m, HasGlobalState env)
    => ConduitT ExtractedPacket TMPacket m ()
packetProcessorC = awaitForever $ \pkt@(ExtractedPacket oct pusPkt) -> do

    logDebug
        $  display ("packetProcessorC: got packet: " :: Text)
        <> displayShow pkt

    env   <- ask
    model <- getDataModel env
    cfg   <- view getConfig

    -- first check for verifications 
    when (pusType (pusPkt ^. epDU . pusDfh) == 1) $ do
        processVerification pusPkt

    -- if we received a TC Echo just log it 
    when (pusPkt ^. epDU . pusHdr . pusHdrType == PUSTC) $ do 
        logDebug $ "Received TC Echo from SCOE: " <> displayShow (pusPkt ^. epDU)

    let def' = getPackeDefinition model pkt
    case def' of
        Just (key, def) -> do
            if def ^. tmpdCheck
                then do
                    logDebug $ "TM Packet Processor: received: " <> display (hexdumpBS oct)
                    checkCRC pusPkt def key oct 
                else yieldM $ processPacket def key pkt
        Nothing -> do
            -- if we have a packet, that is not found in the data model,
            -- we create a new TM packet from it where the data part
            -- is the data part of the PUS packet
            logWarn
                $  "No packet defintion found for packet: APID:"
                <> display (pusPkt ^. epDU . pusHdr . pusHdrAPID)
                <> " Type:"
                <> display (pusType (pusPkt ^. epDU . pusDfh))
                <> " SubType:"
                <> display (pusSubType (pusPkt ^. epDU . pusDfh))
            (timeStamp, _epoch) <- getTimeStamp (pkt ^. extrPacket)

            let param = TMParameter
                    { _pName     = "Content"
                    , _pTime     = timeStamp
                    , _pValue    = TMValue
                                       (TMValOctet (toBS (pusPkt ^. epDU . pusData)))
                                       clearValidity
                    , _pEngValue = Nothing
                    }

                tmpkt = TMPacket
                    { _tmpSPID      = cfgUnknownSPID cfg
                    , _tmpMnemonic  = "UNKNOWN PACKET"
                    , _tmpDescr     = ""
                    , _tmpAPID      = pusPkt ^. epDU . pusHdr . pusHdrAPID
                    , _tmpType      = pusType (pusPkt ^. epDU . pusDfh)
                    , _tmpSubType   = pusSubType (pusPkt ^. epDU . pusDfh)
                    , _tmpPI1       = 0
                    , _tmpPI2       = 0
                    , _tmpERT       = pusPkt ^. epERT
                    , _tmpTimeStamp = timeStamp
                    , _tmpVCID      = pusPkt ^. epVCID
                    , _tmpSSC       = pusPkt ^. epDU . pusHdr . pusHdrSSC
                    , _tmpEvent     = PIDNo
                    , _tmpSource    = pusPkt ^. epSource
                    , _tmpParams    = V.singleton param
                    }

            yield tmpkt
    where 
        checkCRC pusPkt def key oct = do 
            case crcCheck oct of
                Left err ->
                    logError
                        $  "CRC Check on TM packet failed: "
                        <> display err
                        <> ". Packet ignored."
                Right (chk, payload, crcReceived, crcCalcd) -> do
                    if chk
                        then performProcessing pusPkt def key payload
                        else
                            logError
                            $ "CRC Check on TM packet failed: received: "
                            <> display crcReceived
                            <> ", calculated: "
                            <> display crcCalcd
                            <> ". Packet ignored."


        performProcessing pusPkt def key payload = do
            -- process the packet and pass it on 
            yieldM $ processPacket def key (ExtractedPacket payload pusPkt)




-- | Main function to detect the packet identification of a newly received
-- TM PUS Packet.
getPackeDefinition
    :: DataModel -> ExtractedPacket -> Maybe (TMPacketKey, TMPacketDef)
getPackeDefinition model (ExtractedPacket bytes pkt) =
    let pusPkt     = pkt ^. epDU
        pktType    = hdr ^. pusHdrType 
        hdr        = pusPkt ^. pusHdr
        apid       = hdr ^. pusHdrAPID
        dfh        = pusPkt ^. pusDfh
        t          = pusType dfh
        st         = pusSubType dfh
        (pi1, pi2) = case picFind (model ^. dmPacketIdIdx) apid t st of
            Just def -> extractPIVals def bytes
            Nothing  -> (0, 0)
        pktKey = TMPacketKey apid t st pi1 pi2
    in  
        case pktType of 
            PUSTM -> 
                case HT.ilookup (model ^. dmTMPackets) pktKey of
                    Nothing -> Nothing
                    Just p  -> Just (pktKey, p)
            PUSTC -> 
                -- we have a TC Echo Packet from a SCOE
                let def = tcEchoDef apid t st in 
                Just (pktKey, def)







extractPIVals :: TMPIDefs -> ByteString -> (Int64, Int64)
extractPIVals TMPIDefs {..} bytes =
    let pi1 = fromMaybe 0 $ maybe (Just 0) (extractPIVal bytes) _tmpidP1
        pi2 = fromMaybe 0 $ maybe (Just 0) (extractPIVal bytes) _tmpidP2
    in  (pi1, pi2)


extractPIVal :: ByteString -> TMPIDef -> Maybe Int64
extractPIVal bytes TMPIDef {..}
    | _tmpidWidth == 8
    = fromIntegral <$> getValue @Int8 bytes _tmpidOffset BiE
    | _tmpidWidth == 16
    = fromIntegral <$> getValue @Int16 bytes _tmpidOffset BiE
    | _tmpidWidth == 32
    = fromIntegral <$> getValue @Int32 bytes _tmpidOffset BiE
    | _tmpidWidth == 48
    = fromIntegral <$> getValue @Int48 bytes _tmpidOffset BiE
    | _tmpidWidth == 64
    = getValue @Int64 bytes _tmpidOffset BiE
    | otherwise
    = fromIntegral <$> getBitField bytes (toOffset _tmpidOffset) _tmpidWidth




processPacket
    :: (MonadIO m, MonadReader env m, HasGlobalState env)
    => TMPacketDef
    -> TMPacketKey
    -> ExtractedPacket
    -> m TMPacket
processPacket pktDef (TMPacketKey _apid _t _st pi1 pi2) pkt@(ExtractedPacket _ pusDU)
    = do
        (timestamp, epoch) <- getTimeStamp (pkt ^. extrPacket)

        logDebug
            $  display ("TimeStamp: " :: Text)
            <> display timestamp
            <> display (". Getting parameters..." :: Text)

        params' <- getParameters timestamp epoch pktDef pkt

        logDebug $ display ("Got Parameters: " :: Text) <> displayShow params'

        let params = fromMaybe V.empty params'

        let tmPacket = TMPacket { _tmpSPID      = _tmpdSPID pktDef
                                , _tmpMnemonic  = _tmpdName pktDef
                                , _tmpDescr     = _tmpdDescr pktDef
                                , _tmpAPID      = _tmpdApid pktDef
                                , _tmpType      = _tmpdType pktDef
                                , _tmpSubType   = _tmpdSubType pktDef
                                , _tmpPI1       = fromIntegral pi1
                                , _tmpPI2       = fromIntegral pi2
                                , _tmpERT       = pusDU ^. epERT
                                , _tmpTimeStamp = timestamp
                                , _tmpVCID      = pusDU ^. epVCID
                                , _tmpSSC = pusDU ^. epDU . pusHdr . pusHdrSSC
                                , _tmpEvent     = _tmpdEvent pktDef
                                , _tmpSource    = pusDU ^. epSource
                                , _tmpParams    = params
                                }

        -- check for an event
        case _tmpdEvent pktDef of
            PIDNo       -> return ()
            PIDInfo txt -> do
                env <- ask
                let msg =
                        utf8BuilderToText
                            $  display ("SPID: " :: Text)
                            <> display (_tmpdSPID pktDef)
                            <> " APID: "
                            <> display (_tmpdApid pktDef)
                            <> " Type: "
                            <> display (_tmpdType pktDef)
                            <> " SubType: "
                            <> display (_tmpdSubType pktDef)
                            <> " SSC: "
                            <> display (_tmpSSC tmPacket)
                            <> " Msg: "
                            <> display (ST.toText txt)
                liftIO $ raiseEvent env (EVAlarms (EVPacketInfo msg))
            PIDWarning txt -> do
                env <- ask
                let msg =
                        utf8BuilderToText
                            $  display ("SPID: " :: Text)
                            <> display (_tmpdSPID pktDef)
                            <> " APID: "
                            <> display (_tmpdApid pktDef)
                            <> " Type: "
                            <> display (_tmpdType pktDef)
                            <> " SubType: "
                            <> display (_tmpdSubType pktDef)
                            <> " SSC: "
                            <> display (_tmpSSC tmPacket)
                            <> " Msg: "
                            <> display (ST.toText txt)
                liftIO $ raiseEvent env (EVAlarms (EVPacketWarn msg))
            PIDAlarm txt -> do
                env <- ask
                let msg =
                        utf8BuilderToText
                            $  display ("SPID: " :: Text)
                            <> display (_tmpdSPID pktDef)
                            <> " APID: "
                            <> display (_tmpdApid pktDef)
                            <> " Type: "
                            <> display (_tmpdType pktDef)
                            <> " SubType: "
                            <> display (_tmpdSubType pktDef)
                            <> " SSC: "
                            <> display (_tmpSSC tmPacket)
                            <> " Msg: "
                            <> display (ST.toText txt)
                liftIO $ raiseEvent env (EVAlarms (EVPacketAlarm msg))
        return tmPacket


getTimeStamp
    :: (MonadIO m, MonadReader env m, HasPUSState env, HasCorrelationState env)
    => ExtractedDU PUSPacket
    -> m (SunTime, Epoch)
getTimeStamp epu = do
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


getParameters
    :: ( MonadIO m
       , MonadReader env m
       , HasPUSState env
       , HasCorrelationState env
       , HasDataModel env
       , HasLogFunc env
       )
    => SunTime
    -> Epoch
    -> TMPacketDef
    -> ExtractedPacket
    -> m (Maybe (Vector TMParameter))
getParameters timestamp epoch def pkt = do
    let ert = pkt ^. extrPacket . epERT
    case def ^. tmpdParams of
        TMFixedParams paramLocations ->
            getFixedParams timestamp ert epoch def pkt paramLocations
        TMVariableParams tpsd dfhSize varParams ->
            getVariableParams timestamp ert epoch def pkt tpsd dfhSize varParams


getFixedParams
    :: (MonadIO m, MonadReader env m, HasPUSState env, HasCorrelationState env)
    => SunTime
    -> SunTime
    -> Epoch
    -> TMPacketDef
    -> ExtractedPacket
    -> Vector TMParamLocation
    -> m (Maybe (Vector TMParameter))
getFixedParams timestamp ert epoch _def (ExtractedPacket oct' _ep) locs = do
    vs <- V.foldM (go oct') (Just VB.empty) locs
    case vs of
        Just v  -> return . Just . VB.build $ v
        Nothing -> return Nothing
  where
    -- go :: ByteString -> Maybe (VB.Builder TMParameter) -> TMParamLocation -> m (Maybe (VB.Builder TMParameter))
    go oct bldr loc = do
        case _tmplSuperComm loc of
            Just sup -> do
                let newLocs = unroll loc sup
                    func (l, ts) = do
                        v <- getParamValue epoch
                                           ert
                                           oct
                                           (l ^. tmplOffset)
                                           (l ^. tmplParam)
                        case v of
                            Just v' -> do
                                return $ Just TMParameter
                                    { _pName     = _tmplName loc
                                    , _pTime     = ts
                                    , _pValue    = v'
                                    , _pEngValue = Nothing
                                    }
                            Nothing -> return Nothing
                vals <- mapM func newLocs
                case sequence vals of
                    Just vals' -> return (bldr <> Just (VB.foldable vals'))
                    Nothing    -> return Nothing
            Nothing -> do
                val <- getParamValue epoch
                                     ert
                                     oct
                                     (loc ^. tmplOffset)
                                     (loc ^. tmplParam)

                let parTime = if tOffset == nullRelTime
                        then timestamp
                        else timestamp <+> tOffset
                    tOffset = _tmplTime loc

                case val of
                    Just val' -> do
                        let paramValue = TMParameter { _pName = _tmplName loc
                                                     , _pTime     = parTime
                                                     , _pValue    = val'
                                                     , _pEngValue = Nothing
                                                     }
                        return $ bldr <> Just (VB.singleton paramValue)
                    Nothing -> return Nothing

    unroll
        :: TMParamLocation -> SuperCommutated -> [(TMParamLocation, SunTime)]
    unroll loc sup =
        let n = sup ^. scNbOcc
        in  if n <= 1
                then [(loc, timestamp <+> (loc ^. tmplTime))]
                else
                    let newLocs = update sup . replicate (sup ^. scNbOcc) $ loc
                        startTime = timestamp <+> (loc ^. tmplTime)
                        times     = iterate f startTime
                        deltaTime = sup ^. scTdOcc
                        f x = addSpan x deltaTime
                    in  zip newLocs times


    update :: SuperCommutated -> [TMParamLocation] -> [TMParamLocation]
    update _ []       = []
    update _ [locNew] = [locNew]
    update sup (locOld : locNew : rest) =
        let oldOff   = locOld ^. tmplOffset
            oldWidth = getWidth (locOld ^. tmplParam)
            newOff   = oldOff .+. oldWidth .+. sup ^. scLgOcc
            locNew'  = locNew & tmplOffset .~ newOff
        in  locOld : update sup (locNew' : rest)




getVariableParams
    :: ( MonadIO m
       , MonadReader env m
       , HasPUSState env
       , HasCorrelationState env
       , HasDataModel env
       , HasLogFunc env
       )
    => SunTime
    -> SunTime
    -> Epoch
    -> TMPacketDef
    -> ExtractedPacket
    -> Int
    -> Word8
    -> VarParams
    -> m (Maybe (Vector TMParameter))
getVariableParams timestamp ert epoch pktDef (ExtractedPacket oct' ep) _tpsd dfhSize varParams
    = do
        let offset = mkOffset (fromIntegral dfhSize) 0
        a <- go offset varParams
        case a of
            Just (_, pars) -> return (Just (VB.build pars))
            Nothing        -> return Nothing
  where
        -- go :: Offset -> VarParams -> m (Maybe (Offset, VB.Builder TMParameter))
    go !offset VarParamsEmpty = do
        -- traceM "VarParamsEmpty"
        return $ Just (offset, VB.empty)
    -- Normal parameter, just extract the value, recurse for the rest 
    go !offset (VarNormal vpdParDef rest) = do
        -- traceM $ "VarNormal rest: " <> T.pack (show rest)
        let newOffset =
                offset .+. getPaddedWidth parDef .+. vpdParDef ^. tmvpOffset
            extractOffset = offset .+. getPadding parDef
            parDef        = vpdParDef ^. tmvpParam
        val <- getParamValue epoch ert oct' extractOffset parDef

        case val of
            Just val' -> do
                let !param = TMParameter { _pName     = vpdParDef ^. tmvpName
                                         , _pTime     = timestamp
                                         , _pValue    = val'
                                         , _pEngValue = Nothing
                                         }
                --traceM $ "Extracted parameter: " <> T.pack (show param)

                x <- go newOffset rest
                case x of
                    Just (lastOff, next) ->
                        return . Just $ (lastOff, VB.singleton param <> next)
                    Nothing -> return Nothing
            Nothing -> return Nothing

    -- Group repeater. 
    go !offset (VarGroup repeater group rest) = do
        -- traceM $ "VarGroup \n" <> "Repeater:\n" <> T.pack (ppShow repeater) 
        --     <> "\nGroup:\n"<> T.pack (ppShow group) <> "\nRest:\n" 
        --     <> T.pack (ppShow rest)
        let newOffset =
                offset .+. getPaddedWidth parDef .+. repeater ^. tmvpOffset
            extractOffset = offset .+. getPadding parDef
            parDef        = repeater ^. tmvpParam

        -- traceM $ "Offset: " <> textDisplay offset 
        --     <> " PaddedWidth: " <> textDisplay (getPaddedWidth parDef)
        --     <> " VPD Offset: " <> textDisplay (repeater ^. tmvpOffset)
        --     <> " newOffset: " <> textDisplay newOffset
        --     <> " extratOffset: " <> textDisplay extractOffset

        -- The value specifies, how often the group is repeated
        nval' <- getParamValue epoch ert oct' extractOffset parDef

        case nval' of
            Just nval -> do
              -- traceM $ "N: " <> T.pack (ppShow (getInt nval))

                x <- case nval of
                    TMValue (TMValInt x) _ -> processGroup x newOffset group
                    TMValue (TMValUInt x) _ ->
                        processGroup (fromIntegral x) newOffset group
                    TMValue (TMValDouble x) _ ->
                        processGroup (truncate x) newOffset group
                    _ -> go newOffset rest

                case x of
                    Just (lastOff, parVals) -> do
                        let !param = TMParameter
                                { _pName     = repeater ^. tmvpName
                                , _pTime     = timestamp
                                , _pValue    = nval
                                , _pEngValue = Nothing
                                }
                        -- traceM $ "Repeater parameter: " <> T.pack (ppShow param)
                        -- now take the rest of the packet 

                        y <- go lastOff rest
                        case y of
                            Just (lastOff2, parVals2) ->
                                return
                                    (Just
                                        ( lastOff2
                                        , VB.singleton param
                                        <> parVals
                                        <> parVals2
                                        )
                                    )
                            Nothing -> return Nothing
                    Nothing -> return Nothing
            Nothing -> return Nothing
    -- Fixed repeater. 
    go !offset (VarFixed reps group rest) = do
        --traceM $ "VarFixed" <> T.pack (show reps)

        -- reps specifies, how often the group is repeated
        x <- processGroup (fromIntegral reps) offset group
        case x of
            Just (lastOff, parVals) -> do
              -- now take the rest of the packet 
                y <- go lastOff rest
                case y of
                    Just (lastOff2, parVals2) ->
                        return $ Just (lastOff2, parVals <> parVals2)
                    Nothing -> return Nothing
            Nothing -> return Nothing

    go !offset (VarChoice par) = do
        -- traceM "VarChoice"
        let !newOffset = offset .+. getPaddedWidth parDef .+. par ^. tmvpOffset
            !extractOffset = offset .+. getPadding parDef
            parDef = par ^. tmvpParam
        -- The value specifies, how often the group is repeated 
        x' <- getParamValue epoch ert oct' extractOffset parDef
        case x' of
            Just tpsdval -> do
                let !param = TMParameter { _pName     = par ^. tmvpName
                                         , _pTime     = timestamp
                                         , _pValue    = tpsdval
                                         , _pEngValue = Nothing
                                         }
                -- traceM $ "Choice parameter: " <> T.pack (show param)

                choiceRes <- case tpsdval of
                    TMValue (TMValInt x) _ -> processChoice x newOffset
                    TMValue (TMValUInt x) _ ->
                        processChoice (fromIntegral x) newOffset
                    TMValue (TMValDouble x) _ ->
                        processChoice (truncate x) newOffset
                    _ ->
                        return
                            $  Left
                            $  utf8BuilderToText
                            $  display @Text "CHOICE parameter "
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
                            <> display (ep ^. epDU . pusHdr . pusHdrSSC)

                case choiceRes of
                    Left err -> do
                        logWarn $ display err
                        return $ Just (offset, VB.empty)
                    Right (lastOffset, params) -> do
                        return $ Just (lastOffset, VB.singleton param <> params)
            Nothing -> return Nothing
    -- TODO
    go !offset (VarPidRef _ _) = do
        traceM "DEDUCED parameters not yet implemented"
        return (Just (offset, VB.empty))


    -- processGroup :: Int64 -> Offset -> VarParams -> m (Maybe (Offset, VB.Builder TMParameter))
    processGroup 0 offset _ = do
        -- traceM $ "processGroup 0 " <> textDisplay offset
        return $ Just (offset, VB.empty)
    processGroup !n offset params = do
        -- traceM $ "processGroup " <> textDisplay n <> " offset: " <> textDisplay offset

        x <- go offset params
        case x of
            Just (newOff, parVals) -> do

                y <- processGroup (n - 1) newOff params
                case y of
                    Just (lastOff, nextVals) ->
                        return $ Just (lastOff, parVals <> nextVals)
                    Nothing -> return Nothing
            Nothing -> return Nothing


    processChoice !tpsd !offset = do
        -- traceM $ "processChoice " <> T.pack (show tpsd)
        env <- ask
        dm  <- getDataModel env
        let vpds = dm ^. dmVPDStructs
        case HT.ilookup vpds (fromIntegral tpsd) of
            Nothing -> return $ Left
                (  "Choice parameter: could not find TPSD "
                <> T.pack (show tpsd)
                <> " in VPD"
                )
            Just struct -> do
                pars <- go offset struct
                case pars of
                    Just p  -> return (Right p)
                    Nothing -> return $ Left
                        ("Choice parameter: error on extracting data for chosen structure TPSD: "
                        <> T.pack (show tpsd)
                        <> " in VPD"
                        )


getParamValue
    :: (MonadIO m, MonadReader env m, HasPUSState env, HasCorrelationState env)
    => Epoch
    -> SunTime
    -> ByteString
    -> Offset
    -> TMParameterDef
    -> m (Maybe TMValue)
getParamValue epoch ert oct off parDef = do
  -- traceM $ "getParamValue " <> textDisplay off 
    case extractParamValue epoch oct off parDef of
        Just (val, corr) -> do
            if corr == CorrelationYes
                then do
                    case val ^. tmvalValue of
                        TMValTime inTime -> do
                            newTime <- TMValTime <$> correlateTMTime inTime ert
                            return $ Just (val & tmvalValue .~ newTime)
                        _ -> return (Just val)
                else return (Just val)
        Nothing -> return Nothing


extractParamValue
    :: Epoch
    -> ByteString
    -> Offset
    -> TMParameterDef
    -> Maybe (TMValue, Correlate)
extractParamValue epoch oct offset par =
  --trace ("extractParamValue " <> textDisplay offset) $  
    let endian = par ^. fpEndian
    in
        case par ^. fpType of
            ParamInteger w ->
                let v = readIntParam offset (BitSize w) endian
                in  fmap (\v' -> (TMValue v' clearValidity, CorrelationNo)) v
            ParamUInteger w ->
                let v = readUIntParam offset (BitSize w) endian
                in  fmap (\v' -> (TMValue v' clearValidity, CorrelationNo)) v
            ParamDouble dt ->
                let v = readDoubleParam offset endian dt
                in  fmap (\v' -> (TMValue v' clearValidity, CorrelationNo)) v
            ParamString w ->
                let v = readString offset w
                in  Just (TMValue v clearValidity, CorrelationNo)
            ParamOctet w ->
                let v = readOctet offset w
                in  Just (TMValue v clearValidity, CorrelationNo)
            ParamTime tt ct ->
                let v = readTime offset tt
                in  fmap (\v' -> (TMValue v' clearValidity, ct)) v
            ParamDeduced _ ->
                -- TODO handle deduced params
                Just (TMValue TMValNothing clearValidity, CorrelationNo)
            ParamSavedSynthetic ->
                -- TODO handle synthetic params
                Just (TMValue TMValNothing clearValidity, CorrelationNo)


  where
    readIntParam off width endian = if isByteAligned off
        then case width of
            8 ->
                TMValInt
                    .   fromIntegral
                    <$> getValue @Int8 oct (toByteOffset off) endian
            16 ->
                TMValInt
                    .   fromIntegral
                    <$> getValue @Int16 oct (toByteOffset off) endian
            32 ->
                TMValInt
                    .   fromIntegral
                    <$> getValue @Int32 oct (toByteOffset off) endian
            64 -> TMValInt <$> getValue @Int64 oct (toByteOffset off) endian
            w  -> TMValInt <$> getBitFieldInt64 oct offset w endian
        else TMValInt <$> getBitFieldInt64 oct offset width endian

    readUIntParam off width endian = if isByteAligned off
        then case width of
            8 ->
                TMValUInt
                    .   fromIntegral
                    <$> getValue @Word8 oct (toByteOffset off) endian
            16 ->
                TMValUInt
                    .   fromIntegral
                    <$> getValue @Word16 oct (toByteOffset off) endian
            32 ->
                TMValUInt
                    .   fromIntegral
                    <$> getValue @Word32 oct (toByteOffset off) endian
            64 -> TMValUInt <$> getValue @Word64 oct (toByteOffset off) endian
            w  -> TMValUInt <$> getBitFieldWord64 oct offset w endian
        else TMValUInt <$> getBitFieldWord64 oct offset width endian


    readDoubleParam off endian DTDouble = if isByteAligned off
        then TMValDouble <$> getValue @Double oct (toByteOffset off) endian
        else TMValDouble <$> getBitFieldDouble oct (toOffset off) endian
    readDoubleParam off endian DTFloat = if isByteAligned off
        then
            TMValDouble
            .   realToFrac
            <$> getValue @Float oct (toByteOffset off) endian
        else
            TMValDouble
            .   realToFrac
            <$> getBitFieldFloat oct (toOffset off) endian
    readDoubleParam off endian DTMilSingle = if isByteAligned off
        then
            TMValDouble
            .   getMilSingle
            <$> getValue @MILSingle oct (toByteOffset off) endian
        else TMValDouble <$> getBitFieldMilSingle oct (toOffset off) endian
    readDoubleParam off endian DTMilExtended = if isByteAligned off
        then
            TMValDouble
            .   getMilExtended
            <$> getValue @MILExtended oct (toByteOffset off) endian
        else TMValDouble <$> getBitFieldMilExtended oct (toOffset off) endian

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
        in  TMValOctet val
    readOctet off Nothing =
        let val = B.drop (unByteOffset (toByteOffset off)) oct
        in  TMValOctet val

    readTime off (CUC1 False) =
        let !sec = getValue @Word8 oct (toByteOffset off) BiE
            encTime s = mkCUC s False
        in  case sec of
                Just se ->
                    Just $ TMValTime (cucTimeToSunTime epoch (encTime se))
                Nothing -> Nothing
    readTime off (CUC1 True) =
        let !sec = getValue @Int8 oct (toByteOffset off) BiE
            encTime s = mkCUC s True
        in  case sec of
                Just s  -> Just $ TMValTime (cucTimeToSunTime epoch (encTime s))
                Nothing -> Nothing

    readTime off (CUC1_1 False) =
        let !sec = getValue @Word8 oct (toByteOffset off) BiE
            !mic = getValue @Word8 oct (toByteOffset off + 1) BiE
            encTime s m = mkCUC_1 s m False
        in  case (sec, mic) of
                (Just s, Just m) ->
                    Just $ TMValTime (cucTimeToSunTime epoch (encTime s m))
                _ -> Nothing
    readTime off (CUC1_1 True) =
        let !sec = getValue @Int8 oct (toByteOffset off) BiE
            !mic = getValue @Word8 oct (toByteOffset off + 1) BiE
            encTime s m = mkCUC_1 s m True
        in  case (sec, mic) of
                (Just s, Just m) ->
                    Just $ TMValTime (cucTimeToSunTime epoch (encTime s m))
                _ -> Nothing

    readTime off (CUC1_2 False) =
        let !sec = getValue @Word8 oct (toByteOffset off) BiE
            !mic = getValue @Word16 oct (toByteOffset off + 1) BiE
            encTime s m = mkCUC_2 s m False
        in  case (sec, mic) of
                (Just s, Just m) ->
                    Just $ TMValTime (cucTimeToSunTime epoch (encTime s m))
                _ -> Nothing
    readTime off (CUC1_2 True) =
        let !sec = getValue @Int8 oct (toByteOffset off) BiE
            !mic = getValue @Word16 oct (toByteOffset off + 1) BiE
            encTime s m = mkCUC_2 s m True
        in  case (sec, mic) of
                (Just s, Just m) ->
                    Just $ TMValTime (cucTimeToSunTime epoch (encTime s m))
                _ -> Nothing

    readTime off (CUC1_3 False) =
        let !sec = getValue @Word8 oct (toByteOffset off) BiE
            !mic = getValue @Word24 oct (toByteOffset off + 1) BiE
            encTime s m = mkCUC_3 s m False
        in  case (sec, mic) of
                (Just s, Just m) ->
                    Just $ TMValTime $ cucTimeToSunTime epoch (encTime s m)
                _ -> Nothing
    readTime off (CUC1_3 True) =
        let !sec = getValue @Int8 oct (toByteOffset off) BiE
            !mic = getValue @Word24 oct (toByteOffset off + 1) BiE
            encTime s m = mkCUC_3 s m True
        in  case (sec, mic) of
                (Just s, Just m) ->
                    Just $ TMValTime $ cucTimeToSunTime epoch (encTime s m)
                _ -> Nothing

    readTime off (CUC2 False) =
        let !sec = getValue @Word16 oct (toByteOffset off) BiE
            encTime s = mkCUC s False
        in  case sec of
                Just s -> Just $ TMValTime $ cucTimeToSunTime epoch (encTime s)
                _      -> Nothing
    readTime off (CUC2 True) =
        let !sec = getValue @Int16 oct (toByteOffset off) BiE
            encTime s = mkCUC s True
        in  case sec of
                Just s -> Just $ TMValTime $ cucTimeToSunTime epoch (encTime s)
                _      -> Nothing

    readTime off (CUC2_1 False) =
        let !sec = getValue @Word16 oct (toByteOffset off) BiE
            !mic = getValue @Word8 oct (toByteOffset off + 1) BiE
            encTime s m = mkCUC_1 s m False
        in  case (sec, mic) of
                (Just s, Just m) ->
                    Just $ TMValTime $ cucTimeToSunTime epoch (encTime s m)
                _ -> Nothing
    readTime off (CUC2_1 True) =
        let !sec = getValue @Int16 oct (toByteOffset off) BiE
            !mic = getValue @Word8 oct (toByteOffset off + 1) BiE
            encTime s m = mkCUC_1 s m True
        in  case (sec, mic) of
                (Just s, Just m) ->
                    Just $ TMValTime $ cucTimeToSunTime epoch (encTime s m)
                _ -> Nothing

    readTime off (CUC2_2 False) =
        let !sec = getValue @Word16 oct (toByteOffset off) BiE
            !mic = getValue @Word16 oct (toByteOffset off + 1) BiE
            encTime s m = mkCUC_2 s m False
        in  case (sec, mic) of
                (Just s, Just m) ->
                    Just $ TMValTime $ cucTimeToSunTime epoch (encTime s m)
                _ -> Nothing
    readTime off (CUC2_2 True) =
        let !sec = getValue @Int16 oct (toByteOffset off) BiE
            !mic = getValue @Word16 oct (toByteOffset off + 1) BiE
            encTime s m = mkCUC_2 s m True
        in  case (sec, mic) of
                (Just s, Just m) ->
                    Just $ TMValTime $ cucTimeToSunTime epoch (encTime s m)
                _ -> Nothing

    readTime off (CUC2_3 False) =
        let !sec = getValue @Word16 oct (toByteOffset off) BiE
            !mic = getValue @Word24 oct (toByteOffset off + 1) BiE
            encTime s m = mkCUC_3 s m False
        in  case (sec, mic) of
                (Just s, Just m) ->
                    Just $ TMValTime $ cucTimeToSunTime epoch (encTime s m)
                _ -> Nothing
    readTime off (CUC2_3 True) =
        let !sec = getValue @Int16 oct (toByteOffset off) BiE
            !mic = getValue @Word24 oct (toByteOffset off + 1) BiE
            encTime s m = mkCUC_3 s m True
        in  case (sec, mic) of
                (Just s, Just m) ->
                    Just $ TMValTime $ cucTimeToSunTime epoch (encTime s m)
                _ -> Nothing
    readTime off (CUC3 False) =
        let !sec = getValue @Word24 oct (toByteOffset off) BiE
            encTime s = mkCUC s False
        in  case sec of
                Just s -> Just $ TMValTime $ cucTimeToSunTime epoch (encTime s)
                _      -> Nothing
    readTime off (CUC3 True) =
        let !sec = getValue @Int24 oct (toByteOffset off) BiE
            encTime s = mkCUC s True
        in  case sec of
                Just s -> Just $ TMValTime $ cucTimeToSunTime epoch (encTime s)
                _      -> Nothing

    readTime off (CUC3_1 False) =
        let !sec = getValue @Word24 oct (toByteOffset off) BiE
            !mic = getValue @Word8 oct (toByteOffset off + 1) BiE
            encTime s m = mkCUC_1 s m False
        in  case (sec, mic) of
                (Just s, Just m) ->
                    Just $ TMValTime $ cucTimeToSunTime epoch (encTime s m)
                _ -> Nothing
    readTime off (CUC3_1 True) =
        let !sec = getValue @Int24 oct (toByteOffset off) BiE
            !mic = getValue @Word8 oct (toByteOffset off + 1) BiE
            encTime s m = mkCUC_1 s m True
        in  case (sec, mic) of
                (Just s, Just m) ->
                    Just $ TMValTime $ cucTimeToSunTime epoch (encTime s m)
                _ -> Nothing

    readTime off (CUC3_2 False) =
        let !sec = getValue @Word24 oct (toByteOffset off) BiE
            !mic = getValue @Word16 oct (toByteOffset off + 1) BiE
            encTime s m = mkCUC_2 s m False
        in  case (sec, mic) of
                (Just s, Just m) ->
                    Just $ TMValTime $ cucTimeToSunTime epoch (encTime s m)
                _ -> Nothing
    readTime off (CUC3_2 True) =
        let !sec = getValue @Int24 oct (toByteOffset off) BiE
            !mic = getValue @Word16 oct (toByteOffset off + 1) BiE
            encTime s m = mkCUC_2 s m True
        in  case (sec, mic) of
                (Just s, Just m) ->
                    Just $ TMValTime $ cucTimeToSunTime epoch (encTime s m)
                _ -> Nothing

    readTime off (CUC3_3 False) =
        let !sec = getValue @Word24 oct (toByteOffset off) BiE
            !mic = getValue @Word24 oct (toByteOffset off + 1) BiE
            encTime s m = mkCUC_3 s m False
        in  case (sec, mic) of
                (Just s, Just m) ->
                    Just $ TMValTime $ cucTimeToSunTime epoch (encTime s m)
                _ -> Nothing
    readTime off (CUC3_3 True) =
        let !sec = getValue @Int24 oct (toByteOffset off) BiE
            !mic = getValue @Word24 oct (toByteOffset off + 1) BiE
            encTime s m = mkCUC_3 s m True
        in  case (sec, mic) of
                (Just s, Just m) ->
                    Just $ TMValTime $ cucTimeToSunTime epoch (encTime s m)
                _ -> Nothing


    readTime off (CUC4 False) =
        let !sec = getValue @Word32 oct (toByteOffset off) BiE
            encTime s = mkCUC s False
        in  case sec of
                Just s -> Just $ TMValTime $ cucTimeToSunTime epoch (encTime s)
                _      -> Nothing
    readTime off (CUC4 True) =
        let !sec = getValue @Int32 oct (toByteOffset off) BiE
            encTime s = mkCUC s True
        in  case sec of
                Just s -> Just $ TMValTime $ cucTimeToSunTime epoch (encTime s)
                _      -> Nothing

    readTime off (CUC4_1 False) =
        let !sec = getValue @Word32 oct (toByteOffset off) BiE
            !mic = getValue @Word8 oct (toByteOffset off + 1) BiE
            encTime s m = mkCUC_1 s m False
        in  case (sec, mic) of
                (Just s, Just m) ->
                    Just $ TMValTime $ cucTimeToSunTime epoch (encTime s m)
                _ -> Nothing
    readTime off (CUC4_1 True) =
        let !sec = getValue @Int32 oct (toByteOffset off) BiE
            !mic = getValue @Word8 oct (toByteOffset off + 1) BiE
            encTime s m = mkCUC_1 s m True
        in  case (sec, mic) of
                (Just s, Just m) ->
                    Just $ TMValTime $ cucTimeToSunTime epoch (encTime s m)
                _ -> Nothing

    readTime off (CUC4_2 False) =
        let !sec = getValue @Word32 oct (toByteOffset off) BiE
            !mic = getValue @Word16 oct (toByteOffset off + 1) BiE
            encTime s m = mkCUC_2 s m False
        in  case (sec, mic) of
                (Just s, Just m) ->
                    Just $ TMValTime $ cucTimeToSunTime epoch (encTime s m)
                _ -> Nothing
    readTime off (CUC4_2 True) =
        let !sec = getValue @Int32 oct (toByteOffset off) BiE
            !mic = getValue @Word16 oct (toByteOffset off + 1) BiE
            encTime s m = mkCUC_2 s m True
        in  case (sec, mic) of
                (Just s, Just m) ->
                    Just $ TMValTime $ cucTimeToSunTime epoch (encTime s m)
                _ -> Nothing

    readTime off (CUC4_3 False) =
        let !sec = getValue @Word32 oct (toByteOffset off) BiE
            !mic = getValue @Word24 oct (toByteOffset off + 1) BiE
            encTime s m = mkCUC_3 s m False
        in  case (sec, mic) of
                (Just s, Just m) ->
                    Just $ TMValTime $ cucTimeToSunTime epoch (encTime s m)
                _ -> Nothing
    readTime off (CUC4_3 True) =
        let !sec = getValue @Int32 oct (toByteOffset off) BiE
            !mic = getValue @Word24 oct (toByteOffset off + 1) BiE
            encTime s m = mkCUC_3 s m True
        in  case (sec, mic) of
                (Just s, Just m) ->
                    Just $ TMValTime $ cucTimeToSunTime epoch (encTime s m)
                _ -> Nothing

    readTime off UxTime =
        let !sec = getValue @Word32 oct (toByteOffset off) BiE
            !mic = getValue @Word32 oct (toByteOffset off + 1) BiE
            encTime s m = mkCUCTime (fromIntegral s) (fromIntegral m) False
        in  case (sec, mic) of
                (Just s, Just m) ->
                    Just $ TMValTime $ cucTimeToSunTime epoch (encTime s m)
                _ -> Nothing

    readTime off (CDS8 _) =
        let !days  = getValue oct (toByteOffset off) BiE
            !milli = getValue oct (toByteOffset off + 2) BiE
            !micro = getValue oct (toByteOffset off + 6) BiE
            encTime d mi mc = mkCDSTime d mi (Just mc)
            st d mi mc = cdsTimeToSunTime epoch (encTime d mi mc)
        in  case (days, milli, micro) of
                (Just d, Just mi, Just mc) -> Just $ TMValTime (st d mi mc)
                _                          -> Nothing
    readTime off (CDS6 _) =
        let !days  = getValue oct (toByteOffset off) BiE
            !milli = getValue oct (toByteOffset off + 2) BiE
            encTime d mi = mkCDSTime d mi Nothing
            st d mi = cdsTimeToSunTime epoch (encTime d mi)
        in  case (days, milli) of
                (Just d, Just mi) -> Just $ TMValTime (st d mi)
                _                 -> Nothing



processVerification
    :: (MonadReader env m, MonadIO m, HasVerif env, HasLogFunc env)
    => ExtractedDU PUSPacket
    -> m ()
processVerification pusPkt = do
    let subType = pusSubType (pusPkt ^. epDU . pusDfh)
    logDebug $ "Processing Verification (1," <> display subType <> ")"
    case subType of
        1 -> do -- TM Acceptance Success
            processVerifData pusPkt
                             subType
                             verifDataParser
                             requestVerifyTMA
                             StTmSuccess
        2 -> do -- TM Acceptance Failure
            processVerifData pusPkt
                             subType
                             verifDataParser
                             requestVerifyTMA
                             StTmFail
        3 -> do -- TM Start Success
            processVerifData pusPkt
                             subType
                             verifDataParser
                             requestVerifyTMS
                             StTmSuccess
        4 -> do -- TM Start Failure
            processVerifData pusPkt
                             subType
                             verifDataParser
                             requestVerifyTMS
                             StTmFail
        5 -> do -- TM Progress X Success
            processVerifData pusPkt
                             subType
                             verifProgressParser
                             requestVerifyProgressTM
                             StTmSuccess
        6 -> do -- TM Progress X Failure
            processVerifData pusPkt
                             subType
                             verifProgressParser
                             requestVerifyProgressTM
                             StTmFail
        7 -> do -- TM Completion Success
            processVerifData pusPkt
                             subType
                             verifDataParser
                             requestVerifyTMC
                             StTmSuccess
        8 -> do -- TM Start Failure
            processVerifData pusPkt
                             subType
                             verifDataParser
                             requestVerifyTMC
                             StTmFail
        129 -> return ()
        130 -> return ()
        _ -> logWarn $ "Illegal sub-type for service 1 TM: " <> display subType
    return ()


processVerifData
    :: (MonadReader env m, MonadIO m, HasLogFunc env)
    => ExtractedDU PUSPacket
    -> PUSSubType
    -> A.Parser t1
    -> (env -> t1 -> TMStage -> IO ())
    -> TMStage
    -> m ()
processVerifData pusPkt subType p verifF status = do
    env <- ask
    case A.parseOnly p (toBS (pusPkt ^. epDU . pusData)) of
        Right val -> liftIO $ verifF env val status
        Left err ->
            logError
                $  "Could not extract Verification data out of (1,"
                <> display subType
                <> ") packet. Error: "
                <> display (T.pack err)
                <> displayShow (pusPkt ^. epDU)



verifDataParser :: A.Parser (PktID, SeqControl)
verifDataParser = do
    p <- A.anyWord16be
    s <- A.anyWord16be
    return (PktID p, SeqControl s)


verifProgressParser :: A.Parser (PktID, SeqControl, Word8)
verifProgressParser = do
    p   <- A.anyWord16be
    s   <- A.anyWord16be
    idx <- A.anyWord8
    return (PktID p, SeqControl s, idx)
