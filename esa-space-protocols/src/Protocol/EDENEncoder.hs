{-# LANGUAGE TemplateHaskell #-}
module Protocol.EDENEncoder
  ( EdenState
  , defaultEdenState
  , createEdenMsgC
  , edenStateOrigin
  , edenRqstID 

  )
where


import           RIO
import qualified RIO.ByteString                as B

import Conduit as C

import           Control.Lens                   ( makeLenses )

import Data.PUS.PUSPacketEncoder
    ( encPktEncoded, encPktRequest, EncodedPUSPacket )
import Data.PUS.TCRequest
    ( TCRequestBody(TCDir, TCCommand, _tcReqPacket, _tcDestination,
                    _tcReqTransMode, _tcReqMAPID, _tcDirDestination, _tcDirDirective),
      TCRequest,
      tcReqPayload,
      tcReqVCID,
      tcReqIsSpace )
import Data.PUS.TCFrameTypes
    ( encTcFrameData, encTcFrameRequest, EncodedTCFrame )
import Data.PUS.CLTU ( EncodedCLTU(cltuRequest, cltuEncoded) )

import           Protocol.EDEN
import Protocol.ProtocolSwitcher ( QueueMsg(..) )

import General.Time ( SunTime, edenTime, getCurrentTime )
import General.PUSTypes ( VCID(getVCID), mkMAPID, MAPID(getMAPID) )





data EdenState = EdenState {
  _edenStateOrigin :: !EdenTCOrigin
  , _edenRqstID :: !Word32
  }
makeLenses ''EdenState

-- | The default state of the EDEN pipeline
defaultEdenState :: EdenState
defaultEdenState =
  EdenState { _edenStateOrigin = EdenOriginCCS, _edenRqstID = 0 }


-- | Conduit for converting a 'QueueMsg' into a 'EdenMessage' for further encoding. This can 
-- be done on 3 protocol layers: packet, frame and CLTU level. The 'QueueMsg' determines which 
-- protocol level is used.
createEdenMsgC :: (MonadIO m) => ConduitT QueueMsg EdenMessage m () 
createEdenMsgC = do 
  var <- newTVarIO defaultEdenState
  awaitForever $ \case 
    EQPacket pkt -> do 
      res <- packetToEDEN var pkt 
      forM_ res yield
    EQFrame frame -> do 
      res <- frameToEDEN var frame 
      forM_ res yield
    EQCLTU cltu -> do 
      res <- cltuToEDEN var cltu 
      forM_ res yield


packetToEDEN
  :: (MonadIO m) => TVar EdenState -> EncodedPUSPacket -> m (Maybe EdenMessage)
packetToEDEN var encPkt = do
  case encPkt ^. encPktEncoded of
    Nothing     -> return Nothing
    Just binPkt -> do
      now <- liftIO getCurrentTime
      atomically $ do
        st <- readTVar var
        let edenMessage = createMessage now st (encPkt ^. encPktRequest) EdenTcPacket binPkt
            newSt       = st & over edenRqstID (+1)
        writeTVar var newSt
        return (Just edenMessage)


frameToEDEN
  :: (MonadIO m) => TVar EdenState -> EncodedTCFrame -> m (Maybe EdenMessage)
frameToEDEN var encFrame = do
  now <- liftIO getCurrentTime
  atomically $ do
    st <- readTVar var
    let edenMessage = createMessage now st (encFrame ^. encTcFrameRequest) EdenTcFrame (encFrame ^. encTcFrameData)
        newSt       = st & over edenRqstID (+1)
    writeTVar var newSt
    return (Just edenMessage)


cltuToEDEN
  :: (MonadIO m) => TVar EdenState -> EncodedCLTU -> m (Maybe EdenMessage)
cltuToEDEN var encCltu = do
  now <- liftIO getCurrentTime
  atomically $ do
    st <- readTVar var
    let edenMessage = createMessage now st (cltuRequest encCltu) EdenTcCltu (cltuEncoded encCltu)
        newSt       = st & over edenRqstID (+1)
    writeTVar var newSt
    return (Just edenMessage)




createMessage
  :: SunTime -> EdenState -> TCRequest -> EdenTCType -> ByteString -> EdenMessage
createMessage now st rqst protLevel binPkt =
  let
    msg = EdenMessage { _edenType            = EdenTCType
                      , _edenSubType         = detSubType
                      , _edenField1          = edenEmptyField1
                      , _edenField2          = 0
                      , _edenField3          = 0
                      , _edenDataFieldLength = 0
                      , _edenDataField       = dataField
                      }

    (detSubType, mapid) = case rqst ^. tcReqPayload of
      TCCommand {..} -> 
        if rqst ^. tcReqIsSpace 
          then (EdenSpace, _tcReqMAPID)
          else (EdenSCOE, _tcReqMAPID)
      TCDir {..} -> (EdenSpace, mkMAPID 0)

    dataField = case detSubType of
      EdenSpace -> EdenSpaceTC spaceDataField binPkt
      EdenSCOE  -> EdenSCOETC scoeDataField binPkt
      _         -> trace "EDEN TC unit with unsupported type"
                         (EdenSCOETC scoeDataField B.empty)

    spaceDataField = EdenTcSecHeader
      { _edenSecStructure     = 0
      , _edenSecChannel       = getVCID (rqst ^. tcReqVCID)
      , _edenSecTCType        = protLevel
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
