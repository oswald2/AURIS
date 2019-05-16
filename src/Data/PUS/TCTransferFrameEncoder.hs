{-# LANGUAGE OverloadedStrings
    , BangPatterns
    , GeneralizedNewtypeDeriving
    , DeriveGeneric
    , RecordWildCards
#-}
module Data.PUS.TCTransferFrameEncoder
    (
        tcFrameToCltuC
        , tcSegmentToTransferFrame
    )
where


import RIO

import Conduit

import Data.PUS.TCTransferFrame
import Data.PUS.CLTU
import Data.PUS.Segment
import Data.PUS.TCRequest



tcFrameToCltuC :: (MonadIO m, MonadReader env m, HasLogFunc env) => ConduitT EncodedTCFrame CLTU m ()
tcFrameToCltuC = awaitForever $ \frame -> do
    let new = cltuNew (frame ^. encTcFrameData)
    logDebug $ "New CLTU: " <> displayShow new
    yield new

-- -- | indicates, which type this TC Frame is. AD/BD specifies the protocol mode
-- -- (AD = sequence controlled, BD = expedited), BC is a directive (see 'TCDirective')
-- data TCFrameFlag =
--     FrameAD
--     | FrameBD
--     | FrameBC
--     | FrameIllegal
--     deriving (Eq, Ord, Enum, Show, Read)


-- -- | A TC Transfer Frame
-- data TCTransferFrame = TCTransferFrame {
--     _tcFrameVersion :: !Word8
--     , _tcFrameFlag :: !TCFrameFlag
--     , _tcFrameSCID :: !SCID
--     , _tcFrameVCID :: !VCID
--     , _tcFrameLength :: !Word16
--     , _tcFrameSeq :: !Word8
--     , _tcFrameData :: BS.ByteString
--     } deriving (Eq, Show, Read)


-- data EncodedSegment = EncodedSegment {
--         _encSegSegment :: ByteString
--         , _encSegFlag :: SegmentationFlags
--         , _encSeqSegNr :: Word32
--         , _encSeqRequest :: TCRequest
--     }



tcSegmentToTransferFrame :: Monad m => ConduitT EncodedSegment TCTransferFrame m ()
tcSegmentToTransferFrame = awaitForever $ \segm -> do
    let frame = TCTransferFrame 0 FrameBD (rqst ^. tcReqSCID) (rqst ^. tcReqVCID) 0 0 (segm ^. encSegSegment)
        rqst = segm ^. encSegRequest
    yield frame