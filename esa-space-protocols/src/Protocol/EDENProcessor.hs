module Protocol.EDENProcessor
  ( edenMessageProcessorC
  )
where

import           RIO
import qualified RIO.Text                      as T
import qualified RIO.HashMap                   as HM
import           Conduit
import           Data.Attoparsec.ByteString    as A
import           Control.PUS.Classes

import           Data.PUS.ExtractedPUSPacket
import           Data.PUS.MissionSpecific.Definitions
import           Data.PUS.PUSPacket
import           Data.PUS.ExtractedDU
import           Data.PUS.Events

import           Protocol.EDEN
import           Protocol.ProtocolInterfaces

import           General.PUSTypes


edenMessageProcessorC
  :: (MonadIO m, MonadReader env m, HasRaiseEvent env, HasLogFunc env)
  => PUSMissionSpecific
  -> ProtocolInterface
  -> ConduitT EdenMessage ExtractedPacket m ()
edenMessageProcessorC missionSpecific interf = worker HM.empty
 where
  worker counters = do
    x <- await
    case x of
      Just eden -> do
        res <- handleEdenMessage missionSpecific interf counters eden
        case res of 
          Left err -> do 
            logDebug $ display @Text "Error on reception of EDEN message, cancelling connection: " <> display err
            return () 
          Right newCounters -> worker newCounters
      Nothing -> return ()


handleEdenMessage
  :: (MonadIO m, MonadReader env m, HasRaiseEvent env, HasLogFunc env)
  => PUSMissionSpecific
  -> ProtocolInterface
  -> HashMap VCID Word8
  -> EdenMessage
  -> ConduitT EdenMessage ExtractedPacket m (Either Text (HashMap VCID Word8))
handleEdenMessage missionSpecific interf counters eden@EdenMessage { _edenType = EdenTMType }
  = do
    case eden ^. edenDataField of
      -- handle TM via EDEN
      dat@EdenTM {} -> do
        case handleEdenPacket missionSpecific interf dat counters of
          Left err -> do
            env <- ask
            let msg =
                  display @Text "Error decoding EDEN PUS Packet: " <> display err
            logDebug msg
            liftIO $ raiseEvent env $ EVAlarms $ EVPacketAlarm
              (utf8BuilderToText msg)
            return (Left (utf8BuilderToText msg))
          Right (pkt, newCounters) -> do
            yield pkt
            return (Right newCounters)
      _ -> return (Right counters)
handleEdenMessage _missionSpecific _interf counters eden@EdenMessage { _edenType = EdenTCAType }
  = do
    let status = eden ^. edenField2
        tcID   = eden ^. edenField3
    logDebug $ display @Text "TC-A: RqstID: " <> display tcID <> if status == 0
      then display @Text " OK"
      else display @Text " Error: " <> display status
    return (Right counters)
handleEdenMessage _missionSpecific _interf counters eden@EdenMessage { _edenType = EdenTCEType }
  = do 
    logDebug $ display @Text "TC Echo:\n" <> displayShow eden 
    return (Right counters)
handleEdenMessage _missionSpecific _interf counters eden = do 
  logDebug $ display @Text "EDEN Message: Type: " <> displayShow (eden ^. edenType) 
    <> display @Text " SubType: " <> displayShow (eden ^. edenSubType)
  return (Right counters)


handleEdenPacket
  :: PUSMissionSpecific
  -> ProtocolInterface
  -> EdenData
  -> HashMap VCID Word8
  -> Either Text (ExtractedPacket, HashMap VCID Word8)
handleEdenPacket missionSpecific interf EdenTM {..} counters = do
  case parseOnly (match (pusPktParser missionSpecific interf)) _edenTmData of
    Left err -> Left (T.pack err)
    Right (oct, packet) ->
      let epu = ExtractedDU { _epQuality = toFlag Good True
                            , _epERT     = _edenTmSecHeader ^. edenTmSecTime
                            , _epGap     = gap
                            , _epSource  = interf
                            , _epVCID    = vcid
                            , _epDU      = packet ^. protContent
                            }
          extracted = ExtractedPacket oct epu
          vcid      = VCID (_edenTmSecHeader ^. edenTmSecChannel)
          vcfc      = _edenTmSecHeader ^. edenTmSecVCFC
          newMap    = HM.insert vcid vcfc counters
          gap       = case HM.lookup vcid counters of
            Nothing      -> Nothing
            Just oldVCFC -> if oldVCFC + 1 == vcfc
              then Nothing
              else Just (fromIntegral oldVCFC, fromIntegral vcfc)
      in  Right (extracted, newMap)
handleEdenPacket _missionSpecific _ _ _counters = undefined

