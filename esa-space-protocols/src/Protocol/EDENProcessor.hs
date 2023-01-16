module Protocol.EDENProcessor
    ( edenMessageProcessorC
    ) where

import           Conduit
import           Control.PUS.Classes
import           Data.Attoparsec.ByteString    as A
import           RIO
import qualified RIO.HashMap                   as HM
import qualified RIO.Text                      as T

import           Data.PUS.EncTime               ( CucEncoding(..) )
import           Data.PUS.Events                ( Event(EVAlarms)
                                                , EventAlarm(EVPacketAlarm)
                                                )
import           Data.PUS.ExtractedDU
import           Data.PUS.ExtractedPUSPacket
import           Data.PUS.MissionSpecific.Definitions
                                                ( PUSMissionSpecific )
import           Data.PUS.PUSPacket             ( pusPktParser )
import           Data.PUS.Verification

import           Protocol.EDEN
import           Protocol.ProtocolInterfaces    ( ProtocolInterface
                                                , protContent
                                                )

import           General.Hexdump                ( hexdumpBS )
import           General.PUSTypes               ( Good(Good)
                                                , VCID(VCID)
                                                , mkRqstID
                                                , toFlag
                                                )
import           General.Time                   ( edenTimeParser
                                                , nullTime
                                                )
import           General.Types                  ( HasCRC(..)
                                                , HexBytes(toBS)
                                                )

import           Text.Show.Pretty


edenMessageProcessorC
    :: ( MonadIO m
       , MonadReader env m
       , HasRaiseEvent env
       , HasLogFunc env
       , HasVerif env
       )
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
    :: ( MonadIO m
       , MonadReader env m
       , HasRaiseEvent env
       , HasLogFunc env
       , HasVerif env
       )
    => PUSMissionSpecific
    -> ProtocolInterface
    -> HashMap VCID Word8
    -> EdenMessage
    -> ConduitT
           EdenMessage
           ExtractedPacket
           m
           (Either Text (HashMap VCID Word8))
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

handleEdenMessage missionSpecific interf counters eden = do
    case getEdenPacketData (eden ^. edenDataField) of
      -- handle TM via EDEN
        Just dat -> do
            logDebug $ "Received Data: " <> display (hexdumpBS (toBS dat))
            case
                    handleEdenPacket missionSpecific
                                     interf
                                     (eden ^. edenDataField)
                                     counters
                of
                    Left err -> do
                        let
                            msg =
                                display
                                        ("Error decoding EDEN PUS Packet: " :: Text
                                        )
                                    <> display err
                                    <> display (" " :: Text)
                                    <> fromString (ppShow eden)
                        logError msg
                        raiseEvent $ EVAlarms $ EVPacketAlarm
                            (utf8BuilderToText msg)
                        return (Left (utf8BuilderToText msg))
                    Right (pkt, newCounters) -> do
                        yield pkt
                        return (Right newCounters)
        Nothing -> do
            logDebug
                $  "Received EDEN packet with no known structure:\n"
                <> fromString (ppShow eden)
                <> "\n"
                <> display
                       (hexdumpBS
                           (toBS (getEdenRawData (eden ^. edenDataField)))
                       )
            return (Right counters)


handleEdenPacket
    :: PUSMissionSpecific
    -> ProtocolInterface
    -> EdenData
    -> HashMap VCID Word8
    -> Either Text (ExtractedPacket, HashMap VCID Word8)
handleEdenPacket missionSpecific interf EdenTM {..} counters = do
    case
            parseOnly
                (match (pusPktParser missionSpecific Cuc42 HasCRC interf))
                (toBS _edenTmData)
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
            parseOnly
                (match (pusPktParser missionSpecific Cuc42 HasCRC interf))
                (toBS _edenTmScoeData)
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
handleEdenPacket missionSpecific interf EdenSpaceTC {..} counters = do
    case
            parseOnly
                (match (pusPktParser missionSpecific Cuc42 HasCRC interf))
                (toBS _edenSpaceData)
        of
            Left err -> Left (T.pack err)
            Right (oct, packet) ->
                let epu = ExtractedDU { _epQuality = toFlag Good True
                                      , _epERT     = time
                                      , _epGap     = Nothing
                                      , _epSource  = interf
                                      , _epVCID    = IsVCID vcid
                                      , _epDU      = packet ^. protContent
                                      }
                    time =
                        case
                                A.parseOnly
                                    edenTimeParser
                                    (_edenSpaceSecHeader ^. edenSecTime)
                            of
                                Left  _ -> nullTime
                                Right t -> t
                    extracted = ExtractedPacket oct epu
                    vcid      = VCID (_edenSpaceSecHeader ^. edenSecChannel)
                in  Right (extracted, counters)

handleEdenPacket missionSpecific interf EdenSCOETC {..} counters = do
    case
            parseOnly
                (match (pusPktParser missionSpecific Cuc42 HasCRC interf))
                (toBS _edenScoeData)
        of
            Left err -> Left (T.pack err)
            Right (oct, packet) ->
                let epu = ExtractedDU { _epQuality = toFlag Good True
                                      , _epERT     = time
                                      , _epGap     = Nothing
                                      , _epSource  = interf
                                      , _epVCID    = vcid
                                      , _epDU      = packet ^. protContent
                                      }
                    time =
                        case
                                A.parseOnly
                                    edenTimeParser
                                    (_edenSCOESecHeader ^. edenSecScoeTime)
                            of
                                Left  _ -> nullTime
                                Right t -> t
                    extracted = ExtractedPacket oct epu
                    vcid      = IsSCOE
                in  Right (extracted, counters)

handleEdenPacket _missionSpecific _ _ _counters = undefined

