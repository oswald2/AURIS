{-# LANGUAGE OverloadedStrings
    , BangPatterns
    , GeneralizedNewtypeDeriving
    , DeriveGeneric
    , RecordWildCards
#-}
module Data.PUS.TCTransferFrameEncoder
    ( tcFrameToCltuC
    , tcSegmentToTransferFrame
    ) where


import           RIO

import           Conduit

import           Data.PUS.CLTU
import           Data.PUS.Segment
import           Data.PUS.TCFrameTypes
import           Data.PUS.TCRequest

import           General.Types

import           Text.Show.Pretty


tcFrameToCltuC
    :: (MonadIO m, MonadReader env m, HasLogFunc env)
    => ConduitT EncodedTCFrame CLTUInput m ()
tcFrameToCltuC = awaitForever $ \frame -> do
    let new     = cltuNew (toBS (frame ^. encTcFrameData))
        encCltu = CLTUInput new (frame ^. encTcFrameRequest)
    logDebug $ "New CLTU: " <> fromString (ppShow new)
    yield encCltu


tcSegmentToTransferFrame
    :: Monad m => ConduitT EncodedSegment TCFrameTransport m ()
tcSegmentToTransferFrame = awaitForever $ \segm -> do
    let frame = TCTransferFrame 0
                                FrameBD
                                (rqst ^. tcReqSCID)
                                (rqst ^. tcReqVCID)
                                0
                                0
                                (HexBytes (segm ^. encSegSegment))
        rqst = segm ^. encSegRequest
    yield (TCFrameTransport frame rqst)
