{-# LANGUAGE OverloadedStrings
    , BangPatterns
    , GeneralizedNewtypeDeriving
    , DeriveGeneric
    , RecordWildCards
    , NoImplicitPrelude
    , TemplateHaskell
#-}
module Data.PUS.PUSPacketEncoder
    ( EncodedPUSPacket
    , encPktEncoded
    , encPktRequest
    , pusPacketEncoderC
    , tcPktToEncPUSC
    ) where


import           RIO

import           Control.Lens                   ( makeLenses )
import           Control.PUS.Classes

import           Data.Conduit

import           Data.PUS.PUSPacket
--import           Data.PUS.Segment
import           Data.PUS.TCPacketEncoder
import           Data.PUS.TCRequest
import           Data.PUS.Counter



data EncodedPUSPacket = EncodedPUSPacket
    { _encPktEncoded :: Maybe (ByteString, Word16, Word16)
    , _encPktRequest :: TCRequest
    }

makeLenses ''EncodedPUSPacket



pusPacketEncoderC
    :: Monad m => ConduitT (PUSPacket, TCRequest) EncodedPUSPacket m ()
pusPacketEncoderC = awaitForever $ \(pkt, rqst) -> do
    let enc = encodePUSPacket pkt
    yield (EncodedPUSPacket (Just enc) rqst)


tcPktToEncPUSC
    :: (MonadIO m,
        MonadReader env m,
        HasVerif env)
    => SSCCounterMap
    -> ConduitT EncodedTCPacket EncodedPUSPacket m ()
tcPktToEncPUSC hm = do
    x <- await
    case x of
        Nothing    -> return ()
        Just encTC -> do
            let request = encTC ^. encTcRequest
            case encTC ^. encTcPUSContent of
                Just pkt -> do
                    -- first, update the SSC
                    (newHM, ssc) <- lift
                        $ getNextSSC hm (pkt ^. pusHdr . pusHdrAPID)
                    let enc@(_, pktID, seqFlags) = encodePUSPacket newPkt
                        newPkt = pkt & pusHdr . pusHdrSSC .~ ssc

                    -- register this TC for Verification
                    env <- ask
                    liftIO $ registerRequest env request pktID seqFlags

                    -- now pass the packet on to the next stage in encoding
                    yield (EncodedPUSPacket (Just enc) request)
                    tcPktToEncPUSC newHM
                Nothing -> do
                    yield (EncodedPUSPacket Nothing request)
                    tcPktToEncPUSC hm




