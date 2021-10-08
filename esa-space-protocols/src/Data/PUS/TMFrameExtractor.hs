{-|
Module      : Data.PUS.TMFrameExtractor
Description : Extracts the data from a TM Frame into PUS Packets
Copyright   : (c) Michael Oswald, 2019
License     : BSD-3
Maintainer  : michael.oswald@onikudaki.net
Stability   : experimental
Portability : POSIX

This module is about the extraction of the data part of 'TMFrame' s. The
extraction is done per virtual channel (taken from the 'Config') and because
of the used 'Conduit' library automatically handles spillover-packets.
|-}
{-# LANGUAGE
  TemplateHaskell
#-}
module Data.PUS.TMFrameExtractor
    ( extractPktFromTMFramesC
    , tmFrameExtraction
    , tmFrameExtractionChain
    , setupFrameSwitcher
    , tmFrameSwitchVC
    , pusPacketDecodeC
    , tmFrameEncodeC
    , tmFrameDecodeC
    , storeTMFrameC
    , raisePUSPacketC
    , raiseFrameC
    , pusPacketGapCheckC
    , pusPacketStoreC
    , packetStatC
    , frameStatC
    ) where

import qualified Data.IntMap.Strict            as M
import           RIO                     hiding ( (.~)
                                                , (^.)
                                                , to
                                                , view
                                                )
import qualified RIO.ByteString                as B
import qualified RIO.HashMap                   as HM
import qualified RIO.Text                      as T

import           ByteString.StrictBuilder
import           Control.Lens

import           Conduit
import           Conduit.PayloadParser
import           Data.Attoparsec.ByteString     ( Parser )
import qualified Data.Attoparsec.ByteString    as A
import           Data.Conduit.TQueue

import           Control.PUS.Classes

import           Data.Thyme.Clock.POSIX        as Time

import           Data.PUS.Config
import           Data.PUS.EncTime
import           Data.PUS.Events
import           Data.PUS.ExtractedDU
import           Data.PUS.ExtractedPUSPacket
import           Data.PUS.MissionSpecific.Definitions
import           Data.PUS.PUSPacket
import           Data.PUS.SegmentationFlags
import           Data.PUS.Statistics
import           Data.PUS.TMFrame
import           Data.PUS.TMStoreFrame
import           General.APID
import           General.PUSTypes

import           General.Time
import           General.Types

import           General.SizeOf
import           Protocol.ProtocolInterfaces

import           General.Hexdump


data RestartVCException = RestartVCException
    deriving Show

instance Exception RestartVCException




-- | Conduit to encode a 'TMFrame'. Just calls the builder on it and yields the
-- resulting 'ByteString'
tmFrameEncodeC
    :: (MonadReader env m, HasConfig env) => ConduitT TMFrame ByteString m ()
tmFrameEncodeC = awaitForever $ \frame -> do
    cfg <- view getConfig
    let enc    = builderBytes (tmFrameBuilder frame)
        result = tmFrameAppendCRC (cfgTMFrame cfg) enc
    yield result

-- | Conduit to decode a 'TMFrame'. In case the frame cannot be parsed, a
-- 'EV_IllegalTMFrame' event is raised. If the frame could be parsed, first
-- it is checked if it is an idle-frame. Idle-frames are simply discarded.
--
-- In case it is a normal frame, it is CRC-checked. In case the CRC is invalid,
-- a 'EV_IllegalTMFrame' event with an error message is raised.
--
-- If the frame was ok, it is yield'ed to the next conduit.
tmFrameDecodeC
    :: (MonadIO m, MonadReader env m, HasGlobalState env)
    => ConduitT (CDSTime, ByteString) TMStoreFrame m ()
tmFrameDecodeC = do
    env <- ask
    let cfg = env ^. getConfig
    awaitForever $ \(ert, x) -> do
        case A.parseOnly (A.match (tmFrameParser (cfgTMFrame cfg))) x of
            Left err -> do
                let msg = T.pack err
                liftIO $ raiseEvent env (EVAlarms (EVIllegalTMFrame msg))
            Right (bs, frame) -> do

                logDebug
                    $  display ("Received TM Frame: " :: Text)
                    <> displayShow frame

                case tmFrameCheckCRC (cfgTMFrame cfg) bs of
                    Left err -> liftIO
                        $ raiseEvent env (EVTelemetry (EVTMFailedCRC err))
                    Right () -> do
                        let f = TMStoreFrame time frame (HexBytes bs)
                            time =
                                cdsTimeToSunTime (epoch1958 (LeapSeconds 0)) ert
                        yield f


storeTMFrameC
    :: ( MonadIO m
       , MonadReader env m
       , HasDatabase env
       , HasConfig env
       , HasLogFunc env
       )
    => ConduitT (ExtractedDU TMFrame) (ExtractedDU TMFrame) m ()
storeTMFrameC = do
    env <- ask
    let cfg = env ^. getConfig
    if isJust (getDbBackend env) && cfgStoreTMFrames cfg
        then awaitForever $ \frame -> do
            logDebug "Storing TM Frame to DB..."
            liftIO $ storeTMFrame env frame
            yield frame
        else awaitForever yield


setupFrameSwitcher
    :: (MonadUnliftIO m, MonadThrow m, MonadReader env m, HasGlobalState env)
    => ProtocolInterface
    -> TBQueue ExtractedPacket
    -> m (Async (), IntMap (TBQueue TMStoreFrame))
setupFrameSwitcher interf outQueue = do
    st <- ask
    let vcids = cfgVCIDs (st ^. getConfig)

    logDebug $ "Setting up TM Frame Switch for VC IDs: " <> displayShow vcids

    -- generate a TBQueue per virtual channel (input to further processing)
    let proc vcid = do
            queue <- newTBQueueIO 1000
            return (vcid, queue)

    cont <- mapM proc vcids
    -- create the threads
    let
        threadFunc a@(_vcid, queue) = do
            res <-
                tryDeep $ runConduitRes
                    (tmFrameExtractionChain queue outQueue interf)
            case res of
                Left  RestartVCException -> threadFunc a
                Right x                  -> pure x
    thread <- async $ mapConcurrently_ threadFunc cont

    -- create the lookup map to send the contents to the right TBQueue
    let vcMap =
            M.fromList $ map (\(v, q) -> (fromIntegral (getVCID v), q)) cont
    return (thread, vcMap)



-- | Conduit for switching between multiple channels. It queries the 'Config'
-- for a list of configured virtual channels, sets up queues for these channels
-- and a map to switch between these. Per virtual channel a 'tmFrameExtractionChain'
-- is run (each in its own thread).
--
-- When a 'TMFrame' is received, it determines it's virtual channel ID and
-- sends the frame to the right extraction channel for this virtual channel

-- @outQueue is the final 'TBQueue', where extracted PUS Packets are sent
-- for parameter processing
tmFrameSwitchVC
    :: (MonadUnliftIO m, MonadReader env m, HasGlobalState env)
    => IntMap (TBQueue TMStoreFrame)
    -> ConduitT TMStoreFrame Void m ()
tmFrameSwitchVC vcMap = do
    awaitForever $ \frame -> do
        st <- ask
        let
            !vcid = fromIntegral
                $ getVCID (frame ^. tmstFrame . tmFrameHdr . tmFrameVcID)
        case M.lookup vcid vcMap of
            Nothing -> do
                liftIO $ raiseEvent st $ EVAlarms
                    (EVIllegalTMFrame
                        ("Received Frame with VC ID which was not configured: "
                        <> T.pack (show vcid)
                        <> ". Discarding Frame."
                        )
                    )
            Just q -> do
                atomically $ writeTBQueue q frame



raiseFrameC
    :: (MonadIO m, MonadReader env m, HasGlobalState env)
    => ConduitT (ExtractedDU TMFrame) (ExtractedDU TMFrame) m ()
raiseFrameC = awaitForever $ \frame -> do
    env <- ask
    logDebug $ "Raising frame: " <> display frame
    liftIO $ raiseEvent env (EVTelemetry (EVTMFrameReceived frame))
    yield frame



checkFrameCountC
    :: (MonadIO m, MonadReader env m, HasGlobalState env)
    => ProtocolInterface
    -> ConduitT TMStoreFrame (ExtractedDU TMFrame) m ()
checkFrameCountC pIf = go Nothing
  where
    go
        :: (MonadIO m, MonadReader env m, HasGlobalState env)
        => Maybe Word8
        -> ConduitT TMStoreFrame (ExtractedDU TMFrame) m ()
    go lastFC' = do
        x <- await
        case x of
            Nothing     -> return ()
            Just frame' -> do

                logDebug
                    $  display ("checkFrameCountC: " :: Text)
                    <> displayShow frame'

                let frame      = frame' ^. tmstFrame
                    !vcfc      = frame ^. tmFrameHdr . tmFrameVCFC
                    yieldNoGap = do
                        let
                            ep = ExtractedDU
                                { _epQuality = toFlag Good True
                                , _epERT     = frame' ^. tmstTime
                                , _epGap     = Nothing
                                , _epSource  = pIf
                                , _epVCID    = IsVCID
                                    (frame ^. tmFrameHdr . tmFrameVcID)
                                , _epDU      = frame
                                }
                        --traceM "Yield no gap"
                        unless (isIdleTmFrame frame) $ yield ep
                -- traceM $ "TM Frame: VCFC: " <> T.pack (show vcfc)
                case lastFC' of
                    Just lastFC -> do
                        --traceM $ "Last FC: " <> T.pack (show lastFC)
                        -- check, if we have a gap
                        if lastFC + 1 == vcfc
                            then do
                                yieldNoGap
                                go (Just vcfc)
                            else do
                                st <- ask
                                liftIO $ raiseEvent st $ EVTelemetry
                                    (EVTMFrameGap lastFC vcfc)
                                let
                                    ep = ExtractedDU
                                        { _epQuality = toFlag Good True
                                        , _epERT     = frame' ^. tmstTime
                                        , _epGap     =
                                            Just
                                                ( fromIntegral lastFC
                                                , fromIntegral vcfc
                                                )
                                        , _epSource  = pIf
                                        , _epVCID    = IsVCID
                                            (frame ^. tmFrameHdr . tmFrameVcID)
                                        , _epDU      = frame
                                        }
                                --traceM "Yield gap!"
                                logDebug
                                    $  display ("Detected VC Gap: " :: Text)
                                    <> displayShow ep
                                unless (isIdleTmFrame frame) $ yield ep
                                go (Just vcfc)
                    Nothing -> do
                        yieldNoGap
                        go (Just vcfc)




extractPktFromTMFramesC
    :: (MonadIO m, MonadReader env m, HasGlobalState env)
    => PUSMissionSpecific
    -> ProtocolInterface
    -> ConduitT (ExtractedDU TMFrame) ExtDuTMFrame m ()
extractPktFromTMFramesC missionSpecific pIf = loop True B.empty
  where
    loop
        :: (MonadIO m, MonadReader env m, HasGlobalState env)
        => Bool
        -> ByteString
        -> ConduitT (ExtractedDU TMFrame) ExtDuTMFrame m ()
    loop initial spillOverData = do
        logDebug
            $  display ("loop spill len: " :: Text)
            <> displayShow (B.length spillOverData)
            <> display (" spillOverData:\n" :: Text)
            <> display (hexdumpBS spillOverData)
        x <- await
        case x of
            Nothing    -> pure ()
            Just frame -> do
                let frame' = frame ^. epDU
                if isJust (frame ^. epGap)
                    then do
                        -- we have a gap, so process the spill-over packet
                        rejectSpillOver spillOverData
                        let (_prev, rest) = tmFrameGetPrevAndRest frame'
                        logDebug
                            $  display ("gap: prev: " :: Text)
                            <> displayShow (B.length _prev)
                            <> display (" rest: " :: Text)
                            <> displayShow (B.length rest)
                        loop True rest
                    else do
                        -- when we have no gap, just continue processing
                        if initial
                            then do
                                let pdat =
                                        if frame'
                                            ^. tmFrameHdr
                                            .  tmFrameFirstHeaderPtr
                                            == 0
                                        then
                                            frame' ^. tmFrameData
                                        else
                                            let (_prev, rest) =
                                                    tmFrameGetPrevAndRest frame'
                                            in  rest
                                    (pkts, spill) = chunkPackets pdat

                                logDebug
                                    $  displayShow (length pkts)
                                    <> display (" Initial Pkts: " :: Text)
                                    <> displayShow pkts
                                    <> display ("\nspill:\n" :: Text)
                                    <> display (hexdumpBS spill)
                                yieldMany
                                    (map (\i -> ExtDuTMFrame (frame, i)) pkts)
                                loop False spill
                            else do
                                let dat =
                                        spillOverData <> frame' ^. tmFrameData
                                    (pkts, spill) = chunkPackets dat
                                logDebug
                                    $  displayShow (length pkts)
                                    <> display (" Pkts: " :: Text)
                                    <> displayShow pkts
                                    <> display ("\nspill:\n" :: Text)
                                    <> display (hexdumpBS spill)
                                yieldMany
                                    (map (\i -> ExtDuTMFrame (frame, i)) pkts)
                                loop False spill

    rejectSpillOver
        :: (MonadIO m, MonadReader env m, HasGlobalState env)
        => ByteString
        -> ConduitT (ExtractedDU TMFrame) ExtDuTMFrame m ()
    rejectSpillOver sp = do
        env <- ask
        if B.length sp >= fixedSizeOf @PUSHeader
            then do
                case A.parseOnly pusPktHdrLenOnlyParser sp of
                    Left _err ->
                        liftIO
                            $ raiseEvent env
                            $ EVTelemetry
                            $ EVTMGarbledSpillOver (B.unpack sp)
                    Right pktLen -> do
                        let len =
                                fromIntegral (pktLen + 1)
                                    - (B.length sp - fixedSizeOf @PUSHeader)
                            newSp = sp <> B.replicate len 0
                        case
                                A.parseOnly
                                    (pusPktParser missionSpecific pIf)
                                    newSp
                            of
                                Left _err ->
                                    liftIO
                                        $ raiseEvent env
                                        $ EVTelemetry
                                        $ EVTMGarbledSpillOver (B.unpack newSp)
                                Right pusPkt ->
                                    liftIO
                                        $ raiseEvent env
                                        $ EVTelemetry
                                        $ EVTMRejectedSpillOverPkt
                                              (pusPkt ^. protContent)
            else do
                liftIO $ raiseEvent env $ EVTelemetry $ EVTMRejectSpillOver
                    (B.unpack sp)




data PktKey = PktKey !APID !SSC
    deriving (Eq, Generic)

instance Hashable PktKey


data IntermediatePacket = IntermediatePacket
    { impHeader  :: PUSHeader
    , impBody    :: !ByteString
    , impLastLen :: !Word16
    , impSegFlag :: !SegmentationFlags
    , impQuality :: Flag Good
    }

type PktStore = HashMap PktKey IntermediatePacket


data PacketPart = PacketPart
    { ppHeader    :: !PUSHeader
    , ppHeaderBin :: !ByteString
    , ppBody      :: !ByteString
    }


-- | An attoparsec parser for segmented packets. Just parses
-- the header and returns a packet part. Depending on the
-- segmentation flags in the header, the packet part is
-- either a complete standalone packet or a segment
segmentedPacketParser :: TMSegmentLen -> Parser PacketPart
segmentedPacketParser segLen = do
    (hdrBin, hdr) <- A.match pusPktHdrParser
    case hdr ^. pusHdrSeqFlags of
        SegmentStandalone -> do
            let len = hdr ^. pusHdrTcLength + 1
            body <- A.take (fromIntegral len)
            pure (PacketPart hdr hdrBin body)
        _ -> PacketPart hdr hdrBin <$> packetBody hdr segLen


-- | Reconstructs the packet from its parts. In case the packet is
-- unsegmented, just converts it to a 'PUSPacket' and yields it. In
-- case the packet part is a segment, stores it in the 'PktStore' for
-- the assembly. A new packet is started with 'SegmentFirst', continued
-- 'SegmentContinue', where also gap checks are done in case a
-- continuation segment is missing (the packet will be discarded when
-- this is the case) and finished with a 'SegmentLast'.
pktReconstructorC
    :: (MonadIO m, MonadReader env m, HasLogFunc env)
    => PUSMissionSpecific
    -> ProtocolInterface
    -> TMSegmentLen
    -> PktStore
    -> ConduitT (ExtDuTMFrame, PacketPart) ExtractedPacket m ()
pktReconstructorC missionSpecific pIf segLen pktStore = do
    x <- await
    case x of
        Nothing                            -> return ()
        Just (ExtDuTMFrame (epu, _), part) -> do
            let hdr    = ppHeader part
                hdrBin = ppHeaderBin part
                pktKey = mkPktKey hdr
                ert    = epu ^. epERT
                vcid   = epu ^. epVCID
            case hdr ^. pusHdrSeqFlags of
                SegmentStandalone -> do
                    processFinishedPacket missionSpecific
                                          pIf
                                          ert
                                          hdr
                                          hdrBin
                                          vcid
                                          (ppBody part)
                    pktReconstructorC missionSpecific pIf segLen pktStore
                SegmentFirst -> do
                    let newStore = processFirstSegment hdr part pktStore
                    pktReconstructorC missionSpecific pIf segLen newStore
                SegmentContinue -> do
                    newStore <- processContinuationSegment segLen
                                                           pktStore
                                                           pktKey
                                                           part
                    pktReconstructorC missionSpecific pIf segLen newStore
                SegmentLast -> do
                    newStore <- processLastSegment missionSpecific
                                                   pIf
                                                   segLen
                                                   ert
                                                   vcid
                                                   pktStore
                                                   pktKey
                                                   part
                    pktReconstructorC missionSpecific pIf segLen newStore



-- | Creates a 'PktKey' out of a PUSHeader for the 'PktStore'
mkPktKey :: PUSHeader -> PktKey
mkPktKey hdr =
    let pktKey = PktKey apid ssc
        apid   = hdr ^. pusHdrAPID
        ssc    = hdr ^. pusHdrSSC
    in  pktKey


processFinishedPacket
    :: (MonadIO m, MonadReader env m, HasLogFunc env)
    => PUSMissionSpecific
    -> ProtocolInterface
    -> SunTime
    -> PUSHeader
    -> ByteString
    -> EduVCID
    -> ByteString
    -> ConduitT w ExtractedPacket m ()
processFinishedPacket missionSpecific pIf ert hdr hdrBin vcid body = do
    case A.parseOnly (pusPktParserPayload missionSpecific pIf hdr) body of
        Left err -> do
            logWarn $ display ("Error parsing TM packet: " :: Text) <> display
                (T.pack err)
        Right pusPkt -> do
            let ep = ExtractedDU { _epQuality = toFlag Good True
                                 , _epERT     = ert
                                 , _epGap     = Nothing
                                 , _epSource  = pIf
                                 , _epVCID    = vcid
                                 , _epDU      = extrPkt
                                 }
                extrPkt = pusPkt ^. protContent
            -- only pass on the packet if it is not an Idle Pkt
            unless (pusPktIsIdle extrPkt)
                $ yield (ExtractedPacket (hdrBin <> body) ep)

processFirstSegment :: PUSHeader -> PacketPart -> PktStore -> PktStore
processFirstSegment hdr part pktStore =
    let newPkt = IntermediatePacket { impHeader  = hdr
                                    , impBody    = ppBody part
                                    , impLastLen = hdr ^. pusHdrTcLength
                                    , impSegFlag = hdr ^. pusHdrSeqFlags
                                    , impQuality = toFlag Good True
                                    }
        pktKey   = PktKey (hdr ^. pusHdrAPID) (hdr ^. pusHdrSSC)
        newStore = HM.insert pktKey newPkt pktStore
    in  newStore


processContinuationSegment
    :: (MonadIO m, MonadReader env m, HasLogFunc env)
    => TMSegmentLen
    -> PktStore
    -> PktKey
    -> PacketPart
    -> m PktStore
processContinuationSegment segLen pktStore pktKey part = do
    let apid = hdr ^. pusHdrAPID
        ssc  = hdr ^. pusHdrSSC
        hdr  = ppHeader part
    case HM.lookup pktKey pktStore of
        Nothing -> do
            logWarn
                $ "Received continuation segment of TM Packet with no previous segments: APID: "
                <> display apid
                <> " SSC: "
                <> display ssc
            return pktStore
        Just ipkt -> if checkGapsValid segLen ipkt hdr
            then do
                let newPkt = ipkt
                        { impBody    = impBody ipkt `B.append` ppBody part
                        , impLastLen = hdr ^. pusHdrTcLength
                        , impSegFlag = SegmentContinue
                        , impQuality = toFlag Good True
                        }
                    newStore = HM.insert pktKey newPkt pktStore
                return newStore
            else do
                logWarn
                    $  "Detected gap in segmented TM APID: "
                    <> display apid
                    <> " SSC: "
                    <> display ssc
                    <> ". Packet marked as bad."
                let newStore = padGap pktStore pktKey ipkt hdr
                return newStore


padGap :: PktStore -> PktKey -> IntermediatePacket -> PUSHeader -> PktStore
padGap pktStore pktKey ipkt hdr =
    let newPkt = ipkt { impBody    = impBody ipkt `B.append` padding
                      , impLastLen = hdr ^. pusHdrTcLength
                      , impSegFlag = SegmentContinue
                      , impQuality = toFlag Good False
                      }
        padding = B.replicate
            ( fromIntegral (impLastLen ipkt)
            - fromIntegral (hdr ^. pusHdrTcLength)
            )
            0
    in  HM.insert pktKey newPkt pktStore



processLastSegment
    :: (MonadIO m, MonadReader env m, HasLogFunc env)
    => PUSMissionSpecific
    -> ProtocolInterface
    -> TMSegmentLen
    -> SunTime
    -> EduVCID
    -> PktStore
    -> PktKey
    -> PacketPart
    -> ConduitT w ExtractedPacket m PktStore
processLastSegment missionSpecific pIf segLen ert vcid pktStore pktKey part =
    do
        let body   = ppBody part
            apid   = hdr ^. pusHdrAPID
            ssc    = hdr ^. pusHdrSSC
            hdr    = ppHeader part
            hdrBin = ppHeaderBin part

        case HM.lookup pktKey pktStore of
            Nothing -> do
                logWarn
                    $  "Error: no segment of TM packet found APID: "
                    <> display apid
                    <> " SSC: "
                    <> display ssc
                return pktStore
            Just pkt -> if checkGapsValid segLen pkt hdr
                then do
                    let newStore = HM.delete pktKey pktStore
                        newBody  = impBody pkt `B.append` body
                    processFinishedPacket missionSpecific
                                          pIf
                                          ert
                                          hdr
                                          hdrBin
                                          vcid
                                          newBody
                    return newStore
                else do
                    let newStore = padGap pktStore pktKey pkt hdr
                        msg =
                            "Detected gap in segmented TM APID: "
                                <> display apid
                                <> " SSC: "
                                <> display ssc
                                <> ". Packet discarded."
                    logWarn msg
                    return newStore

newtype ExtDuTMFrame = ExtDuTMFrame (ExtractedDU TMFrame, ByteString)

instance GetPayload ExtDuTMFrame where
    getPayload (ExtDuTMFrame x) = snd x


-- | Conduit which takes 'ByteString' and extracts PUS Packets out of
-- this stream. Raises the event 'EV_IllegalPUSPacket' in case a packet
-- could not be parsed. The PUS Packets are wrapped in a 'ProtocolPacket'
-- which also indicates it's source interface
pusPacketDecodeC
    :: (MonadIO m, MonadThrow m, MonadReader env m, HasGlobalState env)
    => ProtocolInterface
    -> ConduitT ExtDuTMFrame ExtractedPacket m ()
pusPacketDecodeC pIf = do
    st <- ask
    let missionSpecific = st ^. getMissionSpecific
        pktStore        = HM.empty
        segLen          = cfgTMSegLength (cfgTMFrame (st ^. getConfig))

    payloadParserC (segmentedPacketParser segLen)
        .| pktReconstructorC missionSpecific pIf segLen pktStore


-- | Stores the extracted packet in the database and then yields it
pusPacketStoreC
    :: (MonadIO m, MonadReader env m, HasDatabase env, HasLogFunc env)
    => ConduitT ExtractedPacket ExtractedPacket m ()
pusPacketStoreC = awaitForever $ \pkt -> do
    env <- ask
    logDebug "Storing PUS Packet to DB..."
    liftIO $ storePUSPacket env (pkt ^. extrPacket)
    yield pkt




pusPacketGapCheckC
    :: (Monad m) => ConduitT ExtractedPacket ExtractedPacket m ()
pusPacketGapCheckC = worker Nothing
  where
    worker sscs = awaitForever $ \pkt -> do
        let hdr     = pkt ^. extrPacket . epDU . pusHdr
            ssc     = hdr ^. pusHdrSSC
            APID ap = hdr ^. pusHdrAPID
            apid    = fromIntegral ap
        case hdr ^. pusHdrType of
            PUSTM -> case sscs of
                Nothing -> do
                    yield pkt
                    worker (Just (M.singleton apid ssc))
                Just old -> do
                    case M.lookup apid old of
                        Nothing -> do
                            yield pkt
                            worker (Just (M.insert apid ssc old))
                        Just oldSSC -> do
                            if oldSSC + 1 == ssc
                                then yield pkt
                                else
                                    yield
                                        (  pkt
                                        &  extrPacket
                                        .  epGap
                                        ?~ ( fromIntegral oldSSC
                                           , fromIntegral ssc
                                           )
                                        )
                            worker (Just (M.insert apid ssc old))
            PUSTC -> do
                yield pkt
                worker sscs


-- | A conduit chain. Reads from a TBQueue which delivers 'TMFrame' s,
-- extracts the packets from the frames, passes them to the PUS Packet
-- parser and passes that to the @outQueue 'TBQueue'.
--
-- Effectively, this queue represents one virtual channel and should
-- run in it's own thread. Multiple of these chains are possible for
-- multiple virtual channels.
--
-- 'tmFrameSwitchVC' does create multiple conduit chains, each in
-- it's own thread and distributes the frames according to their
-- virtual channel ID
tmFrameExtractionChain
    :: (MonadIO m, MonadThrow m, MonadReader env m, HasGlobalState env)
    => TBQueue TMStoreFrame
    -> TBQueue ExtractedPacket
    -> ProtocolInterface
    -> ConduitT () Void m ()
tmFrameExtractionChain queue outQueue interf =
    sourceTBQueue queue .| tmFrameExtraction interf .| sinkTBQueue outQueue


-- | A conduit which calculates the frame statistics
frameStatC
    :: (MonadIO m, MonadReader env m, HasConfig env, HasStats env)
    => ConduitT (ExtractedDU TMFrame) (ExtractedDU TMFrame) m ()
frameStatC = do
    env <- ask
    let !frameSize =
            fromIntegral . cfgMaxTMFrameLen . cfgTMFrame $ env ^. getConfig
    awaitForever $ \meta -> do
        let frameStatVar = getFrameStats env
        now <- liftIO $ getPOSIXTime
        atomically $ modifyTVar' frameStatVar (statNewDU now frameSize)
        yield meta


packetStatC
    :: (MonadIO m, MonadReader env m, HasStats env)
    => ConduitT ExtractedPacket ExtractedPacket m ()
packetStatC = do
    awaitForever $ \meta -> do
        pktStatVar <- getPacketStats <$> ask
        let pktSize = fromIntegral $ B.length (meta ^. extrBytes)
        now <- liftIO $ getPOSIXTime
        atomically $ modifyTVar' pktStatVar (statNewDU now pktSize)
        yield meta

-- | A conduit chain. Reads 'TMFrame' s,
-- extracts the packets from the frames, passes them to the PUS Packet
-- parser and passes that on downstream.
tmFrameExtraction
    :: (MonadIO m, MonadThrow m, MonadReader env m, HasGlobalState env)
    => ProtocolInterface
    -> ConduitT TMStoreFrame ExtractedPacket m ()
tmFrameExtraction interf = do
    st <- ask
    let missionSpecific = st ^. getMissionSpecific
        cfg             = st ^. getConfig

        frameChain      = if isJust (getDbBackend st) && cfgStoreTMFrames cfg
            then
                checkFrameCountC interf
                .| storeTMFrameC
                .| frameStatC
                .| raiseFrameC
            else checkFrameCountC interf .| frameStatC .| raiseFrameC

    let db = getDbBackend st

    if isJust db && cfgStorePUSPackets cfg
        then
            frameChain
            .| extractPktFromTMFramesC missionSpecific interf
            .| pusPacketDecodeC interf
            .| pusPacketStoreC
            .| pusPacketGapCheckC
            .| packetStatC
        else
            frameChain
            .| extractPktFromTMFramesC missionSpecific interf
            .| pusPacketDecodeC interf
            .| pusPacketGapCheckC
            .| packetStatC


checkGapsValid :: TMSegmentLen -> IntermediatePacket -> PUSHeader -> Bool
checkGapsValid segLen imp hdr = case impSegFlag imp of
    SegmentFirst -> True
    SegmentContinue ->
        fromIntegral (impLastLen imp) - tmSegmentLength segLen == fromIntegral
            (hdr ^. pusHdrTcLength)
    SegmentLast ->
        fromIntegral (impLastLen imp) - tmSegmentLength segLen == fromIntegral
            (hdr ^. pusHdrTcLength)
    SegmentStandalone -> True



packetBody :: PUSHeader -> TMSegmentLen -> Parser ByteString
packetBody hdr segLen = do
    let len       = hdr ^. pusHdrTcLength + 1
        lenToTake = min len (fromIntegral (tmSegmentLength segLen))
    A.take (fromIntegral lenToTake)


raisePUSPacketC
    :: (MonadIO m, MonadReader env m, HasGlobalState env)
    => ConduitT ExtractedPacket ExtractedPacket m ()
raisePUSPacketC = do
    env <- ask
    awaitForever $ \p@(ExtractedPacket _ pkt) -> do
        liftIO $ raiseEvent env (EVTelemetry (EVTMPUSPacketReceived pkt))
        yield p
