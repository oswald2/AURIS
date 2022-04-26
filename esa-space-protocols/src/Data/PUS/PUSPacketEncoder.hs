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

import           Data.PUS.Counter
import           Data.PUS.ISL
import           Data.PUS.PUSPacket
--import           Data.PUS.Segment
import           Data.PUS.TCPacketEncoder
import           Data.PUS.TCRequest

import           General.PUSTypes



data EncodedPUSPacket = EncodedPUSPacket
    { _encPktEncoded :: Maybe (ByteString, PktID, SeqControl)
    , _encPktRequest :: TCRequest
    }

makeLenses ''EncodedPUSPacket



pusPacketEncoderC
    :: Monad m => ConduitT (PUSPacket, TCRequest) EncodedPUSPacket m ()
pusPacketEncoderC = awaitForever $ \(pkt, rqst) -> do
    let enc'@(encPkt, pktID, seqFlags) = encodePUSPacket pkt
        enc                            = case rqst ^. tcReqPayload of
            TCCommand {..} -> case _tcWrapInISL of
                Just islHdr -> (encodeISL islHdr encPkt, pktID, seqFlags)
                Nothing     -> enc'
            _ -> enc'
    yield (EncodedPUSPacket (Just enc) rqst)


tcPktToEncPUSC
    :: (MonadIO m, MonadReader env m, HasVerif env)
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
                    let enc'@(encPkt, pktID, seqFlags) = encodePUSPacket newPkt
                        enc = case request ^. tcReqPayload of
                            TCCommand {..} -> case _tcWrapInISL of
                                Just islHdr ->
                                    (encodeISL islHdr encPkt, pktID, seqFlags)
                                Nothing -> enc'
                            _ -> enc'
                        !newPkt  = pkt & pusHdr . pusHdrSSC .~ ssc
                        !newRqst = tcReqSetSSC request ssc

                    -- register this TC for Verification
                    env <- ask
                    liftIO $ registerRequest env newRqst pktID seqFlags

                    -- now pass the packet on to the next stage in encoding
                    yield (EncodedPUSPacket (Just enc) newRqst)
                    tcPktToEncPUSC newHM
                Nothing -> do
                    yield (EncodedPUSPacket Nothing request)
                    tcPktToEncPUSC hm




