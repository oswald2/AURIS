{-# LANGUAGE
    OverloadedStrings
    , BangPatterns
    , NoImplicitPrelude
#-}
module Data.PUS.TMFrameExtractor
    ( extractPktFromTMFramesC
    )
where

import           RIO
import qualified RIO.ByteString                as B
-- import qualified Data.IntMap.Strict            as M

import           Conduit
import           Data.Conduit.Attoparsec
import           Data.Conduit.TQueue

import           Control.PUS.Classes

import           Data.PUS.TMFrame
import           Data.PUS.PUSPacket
import           Data.PUS.Types
import           Data.PUS.PUSPacket
import           Data.PUS.Config

import           Protocol.ProtocolInterfaces

-- type VCMap = M.IntMap ByteString

-- emptyVCMap :: VCMap
-- emptyVCMap = M.empty



-- tmFrameSwitchVC
--     :: (MonadIO m, MonadReader env m, HasGlobalState env)
--     => ConduitT TMFrame TMFrame m ()
-- tmFrameSwitchVC = do
--     st <- ask
--     let vcids = cfgVCIDs (st ^. getConfig)
--     queues <- mapM (\_ -> newTBQueueIO 1000)
--     let map = M.fromList $ zipWith f vcids queues
--         f vcid queue = (fromIntegral (getVCID vcid), queue)
--         entangledPair 



tmFrameExtractionChain
    :: (MonadIO m, MonadReader env m, HasGlobalState env) => 
    TBQueue TMFrame
    -> ProtocolInterface
    -> ConduitT
           i
           (Either ParseError (PositionRange, ProtocolPacket PUSPacket))
           m
           ()
tmFrameExtractionChain queue interf =
    sourceTBQueue queue .| extractPktFromTMFramesC .| pusPacketDecodeC interf


extractPktFromTMFramesC
    :: (MonadIO m, MonadReader env m, HasGlobalState env)
    => ConduitT TMFrame ByteString m ()
extractPktFromTMFramesC = go True
  where
    go initial = do
        x <- await
        case x of
            Nothing    -> pure ()
            Just frame ->
                    -- vcid         = frame ^. tmFrameHdr . tmFrameVcID
                          if initial
                then do
                    let (prev, rest) = tmFrameGetPrevAndRest frame
                    if B.null prev then yield prev else yield rest
                    go False
                else do
                    yield (frame ^. tmFrameData)
                    go False


pusPacketDecodeC
    :: (MonadReader env m, HasMissionSpecific env)
    => ProtocolInterface
    -> ConduitT
           ByteString
           (Either ParseError (PositionRange, ProtocolPacket PUSPacket))
           m
           ()
pusPacketDecodeC interf = do
    missionSpecific <- view getMissionSpecific
    conduitParserEither (pusPktParser missionSpecific interf)
