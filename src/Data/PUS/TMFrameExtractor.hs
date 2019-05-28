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
#-}
module Data.PUS.TMFrameExtractor
    ( extractPktFromTMFramesC
    , tmFrameExtractionChain
    , tmFrameSwitchVC
    , pusPacketDecodeC
    )
where

import           RIO
--import qualified RIO.ByteString                as B
import qualified Data.IntMap.Strict            as M
import qualified RIO.Text                      as T

import           Conduit
import           Data.Conduit.Attoparsec
import           Data.Conduit.TQueue

import           Control.PUS.Classes

import           Data.PUS.TMFrame
import           Data.PUS.Types
import           Data.PUS.PUSPacket
import           Data.PUS.Config
import           Data.PUS.Events

import           Protocol.ProtocolInterfaces


-- | Conduit for switching between multiple channels. It queries the 'Config'
-- for a list of configured virtual channels, sets up queues for these channels
-- and a map to switch between these. Per virtual channel a 'tmFrameExtractionChain'
-- is run (each in its own thread). 
-- 
-- When a 'TMFrame' is received, it determines it's virtual channel ID and 
-- sends the frame to the right extraction channel for this virtual channel

-- @outQueue is the final 'TBQueue', where extracted PUS Packets are sent
-- for de-segmentation and parameter processing
tmFrameSwitchVC
    :: (MonadUnliftIO m, MonadReader env m, HasGlobalState env)
    => ProtocolInterface
    -> TBQueue (ProtocolPacket PUSPacket)
    -> ConduitT (TMFrame, Flag Initial) Void m ()
tmFrameSwitchVC interf outQueue = do
    st <- ask
    let vcids = cfgVCIDs (st ^. getConfig)

    -- generate a TBQueue per virtual channel (input to further processing)
    let proc vcid = do
            queue <- newTBQueueIO 1000
            return (vcid, queue)

    cont <- mapM proc vcids
    -- create the threads
    let threadFunc (vcid, queue) = runConduitRes
            (tmFrameExtractionChain vcid queue outQueue interf)
    lift $ mapConcurrently_ threadFunc cont

    -- create the lookup map to send the contents to the right TBQueue
    let vcMap =
            M.fromList $ map (\(v, q) -> (fromIntegral (getVCID v), q)) cont

    -- This is the conduit itself, the previous was just setup. 
    let
        conduit = awaitForever $ \val@(frame, _initial) -> do
            let !vcid =
                    fromIntegral $ getVCID (frame ^. tmFrameHdr . tmFrameVcID)
            case M.lookup vcid vcMap of
                Nothing -> do
                    liftIO $ raiseEvent st $ EVAlarms
                        (EV_IllegalTMFrame
                            ("Received Frame with VC ID which was not configured: "
                            <> T.pack (show vcid)
                            <> ". Discarding Frame."
                            )
                        )
                    conduit
                Just q -> do
                    atomically $ writeTBQueue q val
                    conduit
    conduit



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
    :: (MonadIO m, MonadReader env m, HasGlobalState env)
    => VCID
    -> TBQueue (TMFrame, Flag Initial)
    -> TBQueue (ProtocolPacket PUSPacket)
    -> ProtocolInterface
    -> ConduitT () Void m ()
tmFrameExtractionChain _vcid queue outQueue interf =
    sourceTBQueue queue
        .| checkFrameCountC
        .| extractPktFromTMFramesC
        .| pusPacketDecodeC interf
        .| sinkTBQueue outQueue


checkFrameCountC
    :: (MonadIO m, MonadReader env m, HasGlobalState env)
    => ConduitT (TMFrame, Flag Initial) (Maybe (TMFrame, Flag Initial)) m ()
checkFrameCountC = do
    var <- newTVarIO 0xFF

    let
        go = awaitForever $ \val@(frame, _initial) -> do
            let !vcfc = frame ^. tmFrameHdr . tmFrameVCFC
            lastFC <- atomically $ readTVar var

            if lastFC + 1 == vcfc
                then do
                    yield (Just val)
                    atomically $ writeTVar var vcfc
                else do
                    st <- ask
                    liftIO $ raiseEvent st $ EVTelemetry
                        (EV_TM_FrameGap lastFC vcfc)
                    yield Nothing
    go



-- | Conduit for extracting the data part (PUS Packets) out of 
-- the TM Frame. 
extractPktFromTMFramesC
    :: (MonadIO m) => ConduitT (Maybe (TMFrame, Flag Initial)) (Maybe ByteString) m ()
extractPktFromTMFramesC =
    -- on initial, we are called the first time. This means that a 
    -- frame could potentially have a header pointer set to non-zero
    -- which means we have to start parsing at the header pointer offset.
    -- from then on, there should be consistent stream of PUS packets
    -- in the data (could also be idle-packets)
    awaitForever $ \inp ->
        case inp of
            Just (frame, initial) -> 
                if toBool initial
                    then if frame ^. tmFrameHdr . tmFrameFirstHeaderPtr == 0
                        then yield (Just (frame ^. tmFrameData))
                        else do
                            let (_prev, rest) = tmFrameGetPrevAndRest frame
                            yield (Just rest)
                    else yield (Just (frame ^. tmFrameData))
            Nothing -> -- now we have a gap skip, we also yield Nothing
                yield Nothing

-- | Conduit which takes 'ByteString' and extracts PUS Packets out of 
-- this stream. Raises the event 'EV_IllegalPUSPacket' in case a packet 
-- could not be parsed.
pusPacketDecodeC
    :: (MonadIO m, MonadReader env m, HasGlobalState env)
    => ProtocolInterface
    -> ConduitT (Maybe ByteString) (ProtocolPacket PUSPacket) m ()
pusPacketDecodeC interf = do
    st <- ask
    let missionSpecific = st ^. getMissionSpecific
    preProc .| conduitParserEither (pusPktParser missionSpecific interf) .| proc st
  where
    proc st = awaitForever $ \inp -> case inp of
        Left err -> do
            liftIO $ raiseEvent st $ EVAlarms
                (EV_IllegalPUSPacket (T.pack (errorMessage err)))
            proc st
        Right (_, tc') -> do
            yield tc'
            proc st
    
    preProc = awaitForever $ \case 
            Nothing -> preProc
            Just x -> yield x
        
