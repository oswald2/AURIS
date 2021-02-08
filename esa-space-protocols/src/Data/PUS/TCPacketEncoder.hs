{-# LANGUAGE OverloadedStrings
    , BangPatterns
    , GeneralizedNewtypeDeriving
    , DeriveGeneric
    , RecordWildCards
    , TemplateHaskell
    , NoImplicitPrelude
#-}
module Data.PUS.TCPacketEncoder
    ( EncodedTCPacket(..)
    , encodeTCPacket
    , encTcPUSContent
    , encTcRequest
    , tcPktEncoderC
    ) where


import           RIO                     hiding ( (.~) )

import           Control.Lens                   ( makeLenses
                                                , (.~)
                                                )

import           Data.Conduit

import           Data.PUS.TCRequest            
import           Data.PUS.TCPacket              ( TCPacket
                                                    ( _tcpAPID
                                                    , _tcpType
                                                    , _tcpSubType
                                                    , _tcpSourceID
                                                    , _tcpParams
                                                    )
                                                )
import           Data.PUS.TCCnc                 ( TCScoe(_tccAPID, _tccParams) )
import           Data.PUS.PUSPacket             ( PUSPacketType(PUSTC)
                                                , PUSHeader(PUSHeader)
                                                , PUSPacket(PUSPacket)
                                                )
import           Data.PUS.SegmentationFlags     ( SegmentationFlags
                                                    ( SegmentStandalone
                                                    )
                                                )
import           Data.PUS.PUSDfh                ( dfhSourceID
                                                , dfhTypes
                                                , dfhAckFlags
                                                , DataFieldHeader
                                                    ( PUSEmptyHeader
                                                    )
                                                )
import           Data.PUS.Parameter             ( encodeParameters
                                                , toSizedParamList
                                                )
import           Data.PUS.MissionSpecific.Definitions
                                                ( pmsTCDataFieldHeader
                                                , PUSMissionSpecific
                                                )

import           General.PUSTypes               ( loadRqstID
                                                , nextRqstID
                                                , saveRqstID
                                                )
import           General.Types                  ( HexBytes(HexBytes) )

import           Verification.Verification


data EncodedTCPacket = EncodedTCPacket
    { _encTcPUSContent :: Maybe PUSPacket
    , _encTcRequest    :: TCRequest
    }
makeLenses ''EncodedTCPacket


encodeTCPacket :: TCPacket -> Verification -> PUSMissionSpecific -> PUSPacket
encodeTCPacket pkt verif missionSpecific =
    let hdr = PUSHeader 0 0 PUSTC True (_tcpAPID pkt) SegmentStandalone 0 0 0
        dfh =
            missionSpecific
                ^. pmsTCDataFieldHeader
                &  dfhTypes
                .~ (_tcpType pkt, _tcpSubType pkt)
                &  dfhSourceID
                .~ _tcpSourceID pkt
                & dfhAckFlags .~ (isTMAExpected verif
                                , isTMSExpected verif 
                                , isTMPExpected verif 
                                , isTMCExpected verif)
        payload = encodeParameters (toSizedParamList (_tcpParams pkt))
    in  PUSPacket hdr dfh Nothing (HexBytes payload) True

encodeScoeTCPacket :: TCScoe -> PUSMissionSpecific -> PUSPacket
encodeScoeTCPacket pkt _missionSpecific =
  -- The version must be "3", because this is what classifies a C&C packet. Also,
  -- we have not DFH flag set, as there is no secondary header (no pus type and subtype)
    let hdr = PUSHeader 0 3 PUSTC False (_tccAPID pkt) SegmentStandalone 0 0 0
        dfh = PUSEmptyHeader
        payload = encodeUtf8 (_tccParams pkt)
    in 
      -- We don't encode a CRC normally for C&C protocol packets
        PUSPacket hdr dfh Nothing (HexBytes payload) False




tcPktEncoderC
    :: (MonadIO m, MonadReader env m, HasLogFunc env)
    => PUSMissionSpecific
    -> ConduitT TCRequest EncodedTCPacket m ()
tcPktEncoderC missionSpecific = do
    rqstID <- liftIO loadRqstID
    proc rqstID
    void $ liftIO $ saveRqstID rqstID
  where
    proc rqstID = do
        x <- await
        case x of
            Nothing      -> return ()
            Just request -> do
                let newRqst   = request & tcReqRequestID .~ rqstID
                    newRqstID = nextRqstID rqstID
                case newRqst ^. tcReqPayload of
                    TCCommand {..} -> do
                        logDebug $ "TC Packet: " <> displayShow _tcReqPacket
                        let
                            enc = encodeTCPacket _tcReqPacket
                                                 (newRqst ^. tcReqVerifications)
                                                 missionSpecific
                        logDebug $ "Encoded TC Packet: " <> displayShow enc
                        yield $ EncodedTCPacket (Just enc) newRqst
                    TCDir{} -> yield $ EncodedTCPacket Nothing newRqst
                    TCScoeCommand {..} -> do
                        let enc =
                                encodeScoeTCPacket _tcReqCommand missionSpecific
                        yield $ EncodedTCPacket (Just enc) newRqst
                proc newRqstID



