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
  ) where


import           RIO
import qualified RIO.ByteString                as B
import           Data.Conduit
import           Data.Conduit.Combinators
import qualified Data.List.NonEmpty            as L

import           Data.PUS.Segment
import           Data.PUS.PUSPacketEncoder
import           Data.PUS.TCRequest
import           General.PUSTypes


tcSegmentEncoderC :: Monad m => ConduitT EncodedPUSPacket EncodedSegment m ()
tcSegmentEncoderC = awaitForever $ \pkt -> do
  let rqst  = pkt ^. encPktRequest
      mapid = case rqst ^. tcReqPayload of
        TCCommand {..}  -> _tcReqMAPID
        TCDir{}         -> mkMAPID 0
        TCScoeCommand{} -> mkMAPID 0
      segs = case pkt ^. encPktEncoded of
        Just dat -> mkTCSegments mapid dat
        Nothing  -> mkTCSegments mapid B.empty
      encSegs = encodeSegments rqst segs
  yieldMany (L.toList encSegs)

