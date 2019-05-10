{-# LANGUAGE OverloadedStrings
    , BangPatterns
    , GeneralizedNewtypeDeriving
    , DeriveGeneric
    , RecordWildCards
    , NoImplicitPrelude
#-}
module Data.PUS.SegmentEncoder
    ( Data.PUS.Segment.EncodedSegment
    , tcSegmentEncoderC
    )
where


import           RIO

import           Data.Conduit
import           Data.Conduit.Combinators
import qualified Data.List.NonEmpty            as L

import           Data.PUS.Segment
import           Data.PUS.PUSPacketEncoder
import           Data.PUS.TCRequest


tcSegmentEncoderC :: Monad m => ConduitT EncodedPUSPacket EncodedSegment m ()
tcSegmentEncoderC = awaitForever $ \pkt -> do
    let rqst    = pkt ^. encPktRequest
        segs    = mkTCSegments (rqst ^. tcReqMAPID) (pkt ^. encPktEncoded)
        encSegs = encodeSegments rqst segs
    yieldMany (L.toList encSegs)
