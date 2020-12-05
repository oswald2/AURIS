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
  )
where


import           RIO                     hiding ( (.~) )

import           Control.Lens                   ( makeLenses
                                                , (.~)
                                                )

import           Data.Conduit

import           Data.PUS.TCRequest
import           Data.PUS.TCPacket
import           Data.PUS.PUSPacket
import           Data.PUS.SegmentationFlags
import           Data.PUS.PUSDfh
import           Data.PUS.Parameter
import           Data.PUS.MissionSpecific.Definitions

import           General.PUSTypes

data EncodedTCPacket = EncodedTCPacket {
    _encTcPUSContent :: Maybe PUSPacket
    , _encTcRequest :: TCRequest
}
makeLenses ''EncodedTCPacket


encodeTCPacket :: TCPacket -> PUSMissionSpecific -> PUSPacket
encodeTCPacket pkt missionSpecific =
  let hdr = PUSHeader 0 0 PUSTC True (_tcpAPID pkt) SegmentStandalone 0 0 0
      dfh =
          missionSpecific
            ^. pmsTCDataFieldHeader
            &  dfhTypes
            .~ (_tcpType pkt, _tcpSubType pkt)
            &  dfhSourceID
            .~ _tcpSourceID pkt
      payload = encodeParameters (toSizedParamList (_tcpParams pkt))
  in  PUSPacket hdr dfh Nothing payload



tcPktEncoderC
  :: MonadIO m
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
              let enc = encodeTCPacket _tcReqPacket missionSpecific
              yield $ EncodedTCPacket (Just enc) newRqst
            TCDir{} -> yield $ EncodedTCPacket Nothing newRqst
          proc newRqstID 



