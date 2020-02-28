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
      Just eden -> case eden ^. edenDataField of
          -- handle TM via EDEN
        dat@EdenTM {..} -> do
          case handleEdenPacket missionSpecific interf dat counters of
            Left err -> do
              env <- ask
              let
                msg =
                  display @Text "Error decoding EDEN PUS Packet: " <> display err
              logDebug msg
              liftIO $ raiseEvent env $ EVAlarms $ EVPacketAlarm
                (utf8BuilderToText msg)
            Right (pkt, newCounters) -> do
              yield pkt
              worker newCounters
        _ -> worker counters
    
      Nothing -> return ()



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

