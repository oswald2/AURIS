module Protocol.EDENProcessor
    ( edenMessageProcessorC
    ) where

import           RIO
import qualified RIO.Text                      as T
import qualified RIO.HashMap                   as HM
import           Conduit
import           Data.Attoparsec.ByteString    as A
import           Control.PUS.Classes

import           Data.PUS.ExtractedPUSPacket
import           Data.PUS.MissionSpecific.Definitions
                                                ( PUSMissionSpecific )
import           Data.PUS.PUSPacket             ( pusPktParser )
import           Data.PUS.ExtractedDU
import           Data.PUS.Events                ( Event(EVAlarms)
                                                , EventAlarm(EVPacketAlarm)
                                                )

import           Protocol.EDEN
import           Protocol.ProtocolInterfaces    ( protContent
                                                , ProtocolInterface
                                                )

import           General.PUSTypes               ( VCID(VCID)
                                                , toFlag
                                                , Good(Good)
                                                , mkRqstID
                                                )
import           General.Hexdump                ( hexdumpBS )
import           General.Types                  ( HexBytes(unHexBytes) )
import           General.Time                   ( edenTimeParser
                                                , nullTime
                                                )
import           Verification.Verification



edenMessageProcessorC
    :: (MonadIO m, MonadReader env m, HasRaiseEvent env, HasLogFunc env, HasVerif env)
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
                        logError
                            $  display
                                   ("Error on reception of EDEN message, cancelling connection: " :: Text
                                   )
                            <> display err
                        return ()
                    Right newCounters -> worker newCounters
            Nothing -> return ()


handleEdenMessage
    :: (MonadIO m, MonadReader env m, HasRaiseEvent env, HasLogFunc env, HasVerif env)
    => PUSMissionSpecific
    -> ProtocolInterface
    -> HashMap VCID Word8
    -> EdenMessage
    -> ConduitT
           EdenMessage
           ExtractedPacket
           m
           (Either Text (HashMap VCID Word8))
handleEdenMessage missionSpecific interf counters eden@EdenMessage { _edenType = EdenTMType }
    = do
        case eden ^. edenDataField of
          -- handle TM via EDEN
            dat@EdenTM {..} -> do
                logDebug $ "Received TM Data: " <> display
                    (hexdumpBS (unHexBytes _edenTmData))
                case handleEdenPacket missionSpecific interf dat counters of
                    Left err -> do
                        env <- ask
                        let
                            msg =
                                display
                                        ("Error decoding EDEN PUS Packet: " :: Text
                                        )
                                    <> display err
                                    <> display (" " :: Text)
                                    <> displayShow eden
                        logError msg
                        liftIO $ raiseEvent env $ EVAlarms $ EVPacketAlarm
                            (utf8BuilderToText msg)
                        return (Left (utf8BuilderToText msg))
                    Right (pkt, newCounters) -> do
                        yield pkt
                        return (Right newCounters)
            dat@EdenSCOETM {..} -> do
                logDebug $ "Received TM Data: " <> display
                    (hexdumpBS (unHexBytes _edenTmScoeData))
                case handleEdenPacket missionSpecific interf dat counters of
                    Left err -> do
                        env <- ask
                        let
                            msg =
                                display
                                        ("Error decoding EDEN PUS Packet: " :: Text
                                        )
                                    <> display err
                                    <> display (" " :: Text)
                                    <> displayShow eden
                        logError msg
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
        logDebug
            $  display ("TC-A: RqstID: " :: Text)
            <> display tcID
            <> if status == 0
                   then display (" OK" :: Text)
                   else display (" Error: " :: Text) <> display status
        -- Verify the request. Ie send the verification fo the G and T stages to the Verification thread
        env <- ask 
        liftIO $ requestVerifyGT
            env
            (mkRqstID tcID)
            (if status == 0 then StGSuccess else StGFail)
        -- and we're done 
        return (Right counters)

handleEdenMessage _missionSpecific _interf counters eden@EdenMessage { _edenType = EdenTCEType }
    = do
        logDebug $ display ("TC Echo:\n" :: Text) <> displayShow eden
        return (Right counters)

handleEdenMessage _missionSpecific _interf counters eden = do
    logDebug
        $  display ("EDEN Message: Type: " :: Text)
        <> displayShow (eden ^. edenType)
        <> display (" SubType: " :: Text)
        <> displayShow (eden ^. edenSubType)
    return (Right counters)


handleEdenPacket
    :: PUSMissionSpecific
    -> ProtocolInterface
    -> EdenData
    -> HashMap VCID Word8
    -> Either Text (ExtractedPacket, HashMap VCID Word8)
handleEdenPacket missionSpecific interf EdenTM {..} counters = do
    case
            parseOnly (match (pusPktParser missionSpecific interf))
                      (unHexBytes _edenTmData)
        of
            Left err -> Left (T.pack err)
            Right (oct, packet) ->
                let
                    epu = ExtractedDU { _epQuality = toFlag Good True
                                      , _epERT     = time
                                      , _epGap     = gap
                                      , _epSource  = interf
                                      , _epVCID    = IsVCID vcid
                                      , _epDU      = packet ^. protContent
                                      }
                    time =
                        case
                                A.parseOnly
                                    edenTimeParser
                                    (_edenTmSecHeader ^. edenTmSecTime)
                            of
                                Left  _ -> nullTime
                                Right t -> t
                    extracted = ExtractedPacket oct epu
                    vcid      = VCID (_edenTmSecHeader ^. edenTmSecChannel)
                    vcfc      = _edenTmSecHeader ^. edenTmSecVCFC
                    newMap    = HM.insert vcid vcfc counters
                    gap       = case HM.lookup vcid counters of
                        Nothing      -> Nothing
                        Just oldVCFC -> if oldVCFC + 1 == vcfc
                            then Nothing
                            else Just (fromIntegral oldVCFC, fromIntegral vcfc)
                in
                    Right (extracted, newMap)
handleEdenPacket missionSpecific interf EdenSCOETM {..} counters = do
    case
            parseOnly (match (pusPktParser missionSpecific interf))
                      (unHexBytes _edenTmScoeData)
        of
            Left err -> Left (T.pack err)
            Right (oct, packet) ->
                let epu = ExtractedDU { _epQuality = toFlag Good True
                                      , _epERT     = time
                                      , _epGap     = gap
                                      , _epSource  = interf
                                      , _epVCID    = vcid
                                      , _epDU      = packet ^. protContent
                                      }
                    time =
                        case
                                A.parseOnly
                                    edenTimeParser
                                    (_edenTmSecScoeHeader ^. edenTmSecScoeTime)
                            of
                                Left  _ -> nullTime
                                Right t -> t
                    extracted = ExtractedPacket oct epu
                    vcid      = IsSCOE
                    gap       = Nothing
                in  Right (extracted, counters)
handleEdenPacket _missionSpecific _ _ _counters = undefined

