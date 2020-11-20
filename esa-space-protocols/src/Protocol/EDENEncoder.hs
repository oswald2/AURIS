{-# LANGUAGE TemplateHaskell #-}
module Protocol.EDENEncoder
  ( EdenState(..)
  , defaultEdenState
  , packetToEDEN
  )
where


import           RIO
import qualified RIO.ByteString                as B

import           Control.Lens                   ( makeLenses )

import           Data.PUS.PUSPacketEncoder
import           Data.PUS.TCRequest

import           Protocol.EDEN

import           General.Time
import           General.PUSTypes





data EdenState = EdenState {
  _edenStateOrigin :: !EdenTCOrigin
  , _edenRqstID :: !Word32
  }
makeLenses ''EdenState


defaultEdenState :: EdenState
defaultEdenState =
  EdenState { _edenStateOrigin = EdenOriginCCS, _edenRqstID = 0 }


packetToEDEN
  :: (MonadIO m) => TVar EdenState -> EncodedPUSPacket -> m (Maybe EdenMessage)
packetToEDEN var encPkt = do
  case encPkt ^. encPktEncoded of
    Nothing     -> return Nothing
    Just binPkt -> do
      now <- liftIO getCurrentTime
      atomically $ do
        st <- readTVar var
        let edenMessage = createMessage now st encPkt binPkt
            newSt       = st & over edenRqstID (+1)
        writeTVar var newSt
        return (Just edenMessage)



createMessage
  :: SunTime -> EdenState -> EncodedPUSPacket -> ByteString -> EdenMessage
createMessage now st encPkt binPkt =
  let
    msg = EdenMessage { _edenType            = EdenTCType
                      , _edenSubType         = detSubType
                      , _edenField1          = edenEmptyField1
                      , _edenField2          = 0
                      , _edenField3          = 0
                      , _edenDataFieldLength = 0
                      , _edenDataField       = dataField
                      }
    rqst                = encPkt ^. encPktRequest

    (detSubType, mapid) = case rqst ^. tcReqPayload of
      TCCommand {..} -> case _tcCmdType of
        Space -> (EdenSpace, _tcReqMAPID)
        SCOE  -> (EdenSCOE, _tcReqMAPID)
      TCDir {..} -> (EdenSpace, mkMAPID 0)

    dataField = case detSubType of
      EdenSpace -> EdenSpaceTC spaceDataField binPkt
      EdenSCOE  -> EdenSCOETC scoeDataField binPkt
      _         -> trace "EDEN TC unit with unsupported type"
                         (EdenSCOETC scoeDataField B.empty)

    spaceDataField = EdenTcSecHeader
      { _edenSecStructure     = 0
      , _edenSecChannel       = getVCID (rqst ^. tcReqVCID)
      , _edenSecTCType        = EdenTcPacket
      , _edenSecTCID          = _edenRqstID st
      , _edenSecTCOrigin      = _edenStateOrigin st
      , _edenSecTime          = edenTime now
      , _edenSecMapID         = getMAPID mapid
      , _edenSecTCEchoStatus  = 0
      , _edenSecSequenceFlags = EdenSegUnsegmented
      }
    scoeDataField = EdenTcSecSCOEHeader { _edenSecScoeStructure = 2
                                        , _edenSecScoeTCID = _edenRqstID st
                                        , _edenSecScoeTCOrigin = EdenOriginCCS
                                        , _edenSecScoeTime = edenTime now
                                        , _edenSecScoeTCEchoStatus = 0
                                        }
  in
    msg
