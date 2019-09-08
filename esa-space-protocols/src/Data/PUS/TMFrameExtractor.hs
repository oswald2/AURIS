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
    OverloadedStrings
    , BangPatterns
    , NoImplicitPrelude
    , LambdaCase
    , TypeApplications
    , DeriveGeneric
    , DeriveAnyClass
#-}
module Data.PUS.TMFrameExtractor
  ( extractPktFromTMFramesC
  , tmFrameExtraction
  , tmFrameExtractionChain
  , tmFrameSwitchVC
  , pusPacketDecodeC
  , tmFrameEncodeC
  , tmFrameDecodeC
  , storeFrameC
  )
where

import           RIO
import qualified RIO.ByteString                as B
import qualified Data.IntMap.Strict            as M
import qualified RIO.Text                      as T
import qualified RIO.HashMap                   as HM
import           ByteString.StrictBuilder

import           Conduit
import           Data.Conduit.TQueue
import           Data.Conduit.Attoparsec
import           Data.Attoparsec.ByteString     ( Parser )
import qualified Data.Attoparsec.ByteString    as A

import           Control.PUS.Classes

import           Data.PUS.TMFrame
import           Data.PUS.Types
import           Data.PUS.APID
import           Data.PUS.PUSPacket
import           Data.PUS.Config
import           Data.PUS.Events
import           Data.PUS.MissionSpecific.Definitions
import           Data.PUS.SegmentationFlags
import           Data.PUS.ExtractedDU
import           Data.PUS.TMStoreFrame
import           Data.PUS.Time

import           Protocol.ProtocolInterfaces
import           Protocol.SizeOf

--import           General.Hexdump


data RestartVCException = RestartVCException
    deriving (Show)

instance Exception RestartVCException




-- | Conduit to encode a 'TMFrame'. Just calls the builder on it and yields the
-- resulting 'ByteString'
tmFrameEncodeC
  :: (MonadReader env m, HasConfig env) => ConduitT TMFrame ByteString m ()
tmFrameEncodeC = awaitForever $ \frame -> do
  cfg <- view getConfig
  let enc    = builderBytes (tmFrameBuilder frame)
      result = tmFrameAppendCRC cfg enc
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
  => ConduitT ByteString TMStoreFrame m ()
tmFrameDecodeC = do
  cfg <- view getConfig
  conduitParserEither (A.match (tmFrameParser cfg)) .| proc cfg
 where
  proc cfg = awaitForever $ \x -> do
    st <- ask
    case x of
      Left err -> do
        let msg = T.pack (errorMessage err)
        liftIO $ raiseEvent st (EVAlarms (EVIllegalTMFrame msg))
        proc cfg
      Right (_, (bs, frame)) ->
        case tmFrameCheckCRC cfg bs of
          Left err -> do
            liftIO $ raiseEvent st (EVTelemetry (EVTMFailedCRC err))
            proc cfg
          Right () -> do
            let f = TMStoreFrame nullTime frame bs
            yield f
            proc cfg


storeFrameC :: (MonadIO m) => ConduitT TMStoreFrame TMFrame m ()
storeFrameC = awaitForever $ \sf -> do
    let frame = sf ^. tmstFrame
    unless (isIdleTmFrame frame) $ do
        -- store value in database
        yield frame


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
  :: ( MonadUnliftIO m
     , MonadThrow m
     , MonadReader env m
     , HasGlobalState env
     , HasLogFunc env
     )
  => PUSMissionSpecific
  -> ProtocolInterface
  -> TBQueue (ExtractedDU PUSPacket)
  -> ConduitT TMFrame Void m ()
tmFrameSwitchVC missionSpecific interf outQueue = do
  st <- ask
  let vcids = cfgVCIDs (st ^. getConfig)

  -- generate a TBQueue per virtual channel (input to further processing)
  let proc vcid = do
        queue <- newTBQueueIO 1000
        return (vcid, queue)

  cont <- mapM proc vcids
  -- create the threads
  let threadFunc a@(_vcid, queue) = do
        res <- tryDeep $ runConduitRes
          (tmFrameExtractionChain missionSpecific queue outQueue interf)
        case res of
          Left  RestartVCException -> threadFunc a
          Right x                  -> pure x
  lift $ mapConcurrently_ threadFunc cont

  -- create the lookup map to send the contents to the right TBQueue
  let vcMap = M.fromList $ map (\(v, q) -> (fromIntegral (getVCID v), q)) cont

  -- This is the conduit itself, the previous was just setup.
  let
    conduit = awaitForever $ \frame -> do
      let !vcid = fromIntegral $ getVCID (frame ^. tmFrameHdr . tmFrameVcID)
      case M.lookup vcid vcMap of
        Nothing -> do
          liftIO $ raiseEvent st $ EVAlarms
            (EVIllegalTMFrame
              (  "Received Frame with VC ID which was not configured: "
              <> T.pack (show vcid)
              <> ". Discarding Frame."
              )
            )
          conduit
        Just q -> do
          atomically $ writeTBQueue q frame
          conduit
  conduit





checkFrameCountC
  :: (MonadIO m, MonadReader env m, HasGlobalState env)
  => ProtocolInterface
  -> ConduitT TMFrame (ExtractedDU TMFrame) m ()
checkFrameCountC pIf = go Nothing
 where
  go lastFC' = do
    x <- await
    case x of
      Nothing    -> return ()
      Just frame -> do
        --traceM $ "checkFrameCountC: " <> T.pack (show frame)
        let !vcfc = frame ^. tmFrameHdr . tmFrameVCFC
            yieldNoGap = do 
                let ep = ExtractedDU { _epQuality = toFlag Good True
                            , _epGap     = Nothing
                            , _epSource  = pIf
                            , _epDU      = frame
                            }
                --traceM "Yield no gap"
                yield ep
        case lastFC' of
          Just lastFC -> do
              -- check, if we have a gap
            if lastFC + 1 == vcfc
              then do
                yieldNoGap
                go (Just vcfc)
              else do
                st <- ask
                liftIO $ raiseEvent st $ EVTelemetry (EVTMFrameGap lastFC vcfc)
                let ep = ExtractedDU
                      { _epQuality = toFlag Good True
                      , _epGap = Just (fromIntegral lastFC, fromIntegral vcfc)
                      , _epSource  = pIf
                      , _epDU      = frame
                      }
                --traceM "Yield gap!"
                yield ep
                go lastFC'
          Nothing -> do
            yieldNoGap 
            go (Just vcfc)




extractPktFromTMFramesC
  :: (MonadIO m, MonadReader env m, HasGlobalState env)
  => PUSMissionSpecific
  -> ProtocolInterface
  -> ConduitT (ExtractedDU TMFrame) ByteString m ()
extractPktFromTMFramesC missionSpecific pIf = do
  x <- await
  case x of
    Nothing     -> pure ()
    Just frame' -> do
        -- if toBool (frame' ^. epQuality)
        --     then do
        --     else do
        -- on initial, we are called the first time. This means that a
        -- frame could potentially have a header pointer set to non-zero
        -- which means we have to start parsing at the header pointer offset.
        -- from then on, there should be consistent stream of PUS packets
        -- in the data (could also be idle-packets)
      
      --traceM $ "extractPktFromTMFramesC: " <> T.pack (show frame')

      let frame = frame' ^. epDU
          pdat  = if frame ^. tmFrameHdr . tmFrameFirstHeaderPtr == 0
            then frame ^. tmFrameData
            else let (_prev, rest) = tmFrameGetPrevAndRest frame in rest
          (pkts, spill) = chunkPackets pdat

      --traceM $ "Pkts: " <> T.pack (show pkts) <> " spill: " <> hexdumpBS spill
      yieldMany pkts
      loop spill
 where
  loop
    :: (MonadIO m, MonadReader env m, HasGlobalState env)
    => ByteString
    -> ConduitT (ExtractedDU TMFrame) ByteString m ()
  loop spillOverData = do
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
            loop rest
          else do
                -- when we have no gap, just continue processing
            let dat           = spillOverData <> frame' ^. tmFrameData
                (pkts, spill) = chunkPackets dat
            yieldMany pkts
            loop spill

  rejectSpillOver
    :: (MonadIO m, MonadReader env m, HasGlobalState env)
    => ByteString
    -> ConduitT (ExtractedDU TMFrame) ByteString m ()
  rejectSpillOver sp = do
    env <- ask
    if B.length sp >= fixedSizeOf @PUSHeader
      then do
        case A.parseOnly pusPktHdrLenOnlyParser sp of
          Left _err ->
            liftIO $ raiseEvent env $ EVTelemetry $ EVTMGarbledSpillOver
              (B.unpack sp)
          Right pktLen -> do
            let
              len =
                fromIntegral (pktLen + 1)
                  - (B.length sp - fixedSizeOf @PUSHeader)
              newSp = sp <> B.replicate len 0
            case A.parseOnly (pusPktParser missionSpecific pIf) newSp of
              Left _err ->
                liftIO $ raiseEvent env $ EVTelemetry $ EVTMGarbledSpillOver
                  (B.unpack newSp)
              Right pusPkt ->
                liftIO
                  $ raiseEvent env
                  $ EVTelemetry
                  $ EVTMRejectedSpillOverPkt (pusPkt ^. protContent)
      else do
        liftIO $ raiseEvent env $ EVTelemetry $ EVTMRejectSpillOver
          (B.unpack sp)




data PktKey = PktKey !APID !SSC
        deriving(Eq, Generic)

instance Hashable PktKey


data IntermediatePacket = IntermediatePacket {
    impHeader :: PUSHeader
    , impBody :: !ByteString
    , impLastLen :: !Word16
    , impSegFlag :: !SegmentationFlags
    , impQuality :: Flag Good
}

type PktStore = HashMap PktKey IntermediatePacket


data PacketPart = PacketPart {
    ppHeader :: !PUSHeader
    , ppBody :: !ByteString
}


-- | An attoparsec parser for segmented packets. Just parses
-- the header and returns a packet part. Depending on the
-- segmentation flags in the header, the packet part is
-- either a complete standalone packet or a segment
segmentedPacketParser :: TMSegmentLen -> Parser PacketPart
segmentedPacketParser segLen = do
  hdr <- pusPktHdrParser
  case hdr ^. pusHdrSeqFlags of
    SegmentStandalone -> do
      let len = hdr ^. pusHdrTcLength + 1
      body <- A.take (fromIntegral len)
      pure (PacketPart hdr body)
    _ -> PacketPart hdr <$> packetBody hdr segLen


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
  -> ConduitT
       (PositionRange, PacketPart)
       (ExtractedDU PUSPacket)
       m
       ()
pktReconstructorC missionSpecific pIf segLen pktStore = do
  x <- await
  case x of
    Nothing        -> return ()
    Just (_, part) -> do
      let hdr    = ppHeader part
          pktKey = mkPktKey hdr
      case hdr ^. pusHdrSeqFlags of
        SegmentStandalone -> do
          processFinishedPacket missionSpecific pIf hdr (ppBody part)
          pktReconstructorC missionSpecific pIf segLen pktStore
        SegmentFirst -> do
          let newStore = processFirstSegment hdr part pktStore
          pktReconstructorC missionSpecific pIf segLen newStore
        SegmentContinue -> do
          newStore <- processContinuationSegment segLen pktStore pktKey part
          pktReconstructorC missionSpecific pIf segLen newStore
        SegmentLast -> do
          newStore <- processLastSegment missionSpecific
                                         pIf
                                         segLen
                                         pktStore
                                         pktKey
                                         part
          pktReconstructorC missionSpecific pIf segLen newStore



-- | Creates a 'PktKey' out of a PUSHeader for the 'PktStore'
mkPktKey :: PUSHeader -> PktKey
mkPktKey hdr =
  let pktKey = PktKey apid ssc
      apid   = hdr ^. pusHdrTcApid
      ssc    = hdr ^. pusHdrTcSsc
  in  pktKey


processFinishedPacket
  :: (MonadIO m, MonadReader env m, HasLogFunc env)
  => PUSMissionSpecific
  -> ProtocolInterface
  -> PUSHeader
  -> ByteString
  -> ConduitT w (ExtractedDU PUSPacket) m ()
processFinishedPacket missionSpecific pIf hdr body = do
  case A.parseOnly (pusPktParserPayload missionSpecific pIf hdr) body of
    Left err -> do
      logWarn $ display ("Error parsing TM packet: " :: Text) <> display
        (T.pack err)
    Right pusPkt -> do
      let ep = ExtractedDU { _epQuality = toFlag Good True
                           , _epGap     = Nothing
                           , _epSource  = pIf
                           , _epDU      = extrPkt
                           }
          extrPkt = pusPkt ^. protContent
      -- only pass on the packet if it is not an Idle Pkt
      unless (pusPktIsIdle extrPkt) $ yield ep

processFirstSegment :: PUSHeader -> PacketPart -> PktStore -> PktStore
processFirstSegment hdr part pktStore =
  let newPkt = IntermediatePacket { impHeader  = hdr
                                  , impBody    = ppBody part
                                  , impLastLen = hdr ^. pusHdrTcLength
                                  , impSegFlag = hdr ^. pusHdrSeqFlags
                                  , impQuality = toFlag Good True
                                  }
      pktKey   = PktKey (hdr ^. pusHdrTcApid) (hdr ^. pusHdrTcSsc)
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
  let apid = hdr ^. pusHdrTcApid
      ssc  = hdr ^. pusHdrTcSsc
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
        let newPkt = ipkt { impBody    = impBody ipkt `B.append` ppBody part
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
        (fromIntegral (impLastLen ipkt) - fromIntegral (hdr ^. pusHdrTcLength))
        0
  in  HM.insert pktKey newPkt pktStore



processLastSegment
  :: (MonadIO m, MonadReader env m, HasLogFunc env)
  => PUSMissionSpecific
  -> ProtocolInterface
  -> TMSegmentLen
  -> PktStore
  -> PktKey
  -> PacketPart
  -> ConduitT w (ExtractedDU PUSPacket) m (PktStore)
processLastSegment missionSpecific pIf segLen pktStore pktKey part = do
  let body = ppBody part
      apid = hdr ^. pusHdrTcApid
      ssc  = hdr ^. pusHdrTcSsc
      hdr  = ppHeader part

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
        processFinishedPacket missionSpecific pIf hdr newBody
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


-- | Conduit which takes 'ByteString' and extracts PUS Packets out of
-- this stream. Raises the event 'EV_IllegalPUSPacket' in case a packet
-- could not be parsed. The PUS Packets are wrapped in a 'ProtocolPacket'
-- which also indicates it's source interface
pusPacketDecodeC
  :: ( MonadIO m
     , MonadThrow m
     , MonadReader env m
     , HasGlobalState env
     , HasLogFunc env
     )
  => ProtocolInterface
  -> ConduitT ByteString (ExtractedDU PUSPacket) m ()
pusPacketDecodeC pIf = do
  st <- ask
  let missionSpecific = st ^. getMissionSpecific
      pktStore        = HM.empty
      segLen          = cfgTMSegLength (st ^. getConfig)

  conduitParser (segmentedPacketParser segLen)
    .| pktReconstructorC missionSpecific pIf segLen pktStore


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
  :: ( MonadIO m
     , MonadThrow m
     , MonadReader env m
     , HasGlobalState env
     , HasLogFunc env
     )
  => PUSMissionSpecific
  -> TBQueue TMFrame
  -> TBQueue (ExtractedDU PUSPacket)
  -> ProtocolInterface
  -> ConduitT () Void m ()
tmFrameExtractionChain missionSpecific queue outQueue interf =
  sourceTBQueue queue
    .| tmFrameExtraction missionSpecific interf
    .| sinkTBQueue outQueue


-- | A conduit chain. Reads 'TMFrame' s,
-- extracts the packets from the frames, passes them to the PUS Packet
-- parser and passes that on downstream.
tmFrameExtraction
  :: ( MonadIO m
     , MonadThrow m
     , MonadReader env m
     , HasGlobalState env
     , HasLogFunc env
     )
  => PUSMissionSpecific
  -> ProtocolInterface
  -> ConduitT TMFrame (ExtractedDU PUSPacket) m ()
tmFrameExtraction missionSpecific interf =
  checkFrameCountC interf
    .| extractPktFromTMFramesC missionSpecific interf
    .| pusPacketDecodeC interf


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
