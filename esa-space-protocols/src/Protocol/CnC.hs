{-# LANGUAGE
    OverloadedStrings
    , BangPatterns
    , NoImplicitPrelude
#-}
module Protocol.CnC
    ( receiveCnCC
    , sendTCCncC
    , cncToTMPacket
    , cncProcessAcks
    , scoeCommandC
    , SCOECommand(..)
    , generateAckData
    ) where

import           RIO
import qualified RIO.ByteString                as BS
import qualified Data.ByteString.Char8         as BS8
import qualified RIO.Text                      as T
import           ByteString.StrictBuilder       ( builderBytes
                                                , bytes
                                                )

import           Conduit
import           Data.Conduit.Attoparsec
import qualified Data.ByteString.Char8         as BCS
import           Data.Attoparsec.ByteString.Char8
                                               as A
import qualified Data.Attoparsec.Binary        as A
import qualified Data.Vector                   as V
import           Data.Char

import           Data.PUS.PUSPacket
import           Data.PUS.PUSDfh                ( pusSubType
                                                , pusType
                                                )
import           Data.PUS.PUSPacketEncoder
import           Data.PUS.ExtractedDU
import           Data.PUS.ExtractedPUSPacket
import           Data.PUS.MissionSpecific.Definitions
import           Data.PUS.Events
import           Data.PUS.TCRequest
import           Protocol.ProtocolInterfaces    ( protContent
                                                , ProtocolInterface(IfCnc)
                                                , ProtocolPacket(ProtocolPacket)
                                                )

import           General.Time
import           General.PUSTypes
import           General.Types
import General.Hexdump ( hexdumpBS )
import           Control.PUS.Classes

import           Verification.Verification

-- if we have a SCOE packet, and it has a secondary header, it is a binary
-- TC, else an ASCII one.
isASCIICc :: ProtocolPacket PUSPacket -> Bool
isASCIICc (ProtocolPacket (IfCnc _) pusPkt) =
    let hdr = pusPkt ^. pusHdr
    in  hdr ^. pusHdrTcVersion == 3 && not (hdr ^. pusHdrDfhFlag)
isASCIICc _ = False


receiveCnCC
    :: (MonadIO m, MonadReader env m, HasLogFunc env, HasRaiseEvent env)
    => PUSMissionSpecific
    -> ProtocolInterface
    -> ConduitT ByteString (ByteString, ProtocolPacket PUSPacket) m ()
receiveCnCC missionSpecific interf = do
    conduitParserEither (match (pusPktParser missionSpecific interf)) .| sink
  where
    sink = do
        x <- await
        case x of
            Nothing -> return ()
            Just tc -> do
                case tc of
                    Left err -> do
                        env <- ask
                        let msg = "Error decoding CnC packet: "
                                <> displayShow err
                        logError msg
                        liftIO $ raiseEvent
                            env
                            (EVAlarms (EVPacketAlarm (utf8BuilderToText msg)))
                        return ()
                    Right (_, res) -> do
                        yield res
                        sink


cncProcessAcks
    :: (MonadIO m, MonadReader env m, HasLogFunc env, HasVerif env)
    => ProtocolInterface
    -> TBQueue ExtractedPacket
    -> Maybe SSC
    -> ConduitT (ByteString, ProtocolPacket PUSPacket) Void m ()
cncProcessAcks interf queue oldSSC = do
    x <- await
    case x of
        Just (byts, protPkt) -> do
            let pkt = protPkt ^. protContent
            if pkt ^. pusHdr . pusHdrTcVersion == 3 && not
                (pkt ^. pusHdr . pusHdrDfhFlag)
            then do
                -- we have most probably an ACK packet
                processAsciiAck protPkt
                cncProcessAcks interf queue oldSSC
            else
                do
                    -- we have a binary ACK packet
                    processBinaryAck protPkt
                    -- forward the ack packet also to the TM packet processing
                    (newPkt, newSSC) <- convertCncToTMPacket (byts, protPkt)
                                                             interf
                                                             oldSSC
                    atomically $ writeTBQueue queue newPkt
                    cncProcessAcks interf queue (Just newSSC)
        Nothing -> return ()

processBinaryAck
    :: (MonadReader env m, MonadIO m, HasLogFunc env, HasVerif env)
    => ProtocolPacket PUSPacket
    -> m ()
processBinaryAck protPkt = do
    let pusPkt = protPkt ^. protContent
        t      = pusType (pusPkt ^. pusDfh)
        st     = pusSubType (pusPkt ^. pusDfh)
    case t of
        1 -> do
            env <- ask
            case st of
                129 -> do
                  -- ACK
                    logDebug $ "Received C&C ACK: " <> displayShow pusPkt
                    parseAndVerify env pusPkt StGSuccess
                130 -> do
                  -- NAK
                    logDebug $ "Received C&C NAK: " <> displayShow pusPkt
                    parseAndVerify env pusPkt StGFail
                _ -> return ()
        _ -> return ()
  where
    parseAndVerify env pusPkt status = do
        let dat = pusPkt ^. pusData
        case parseOnly ackDataParser (toBS dat) of
            Left err ->
                logWarn
                    $  "Could not parse C&C ACK data from pkt: "
                    <> display (T.pack err)
                    <> " packet: "
                    <> displayShow pusPkt
            Right val -> liftIO $ requestVerifyGTCnC env val status


ackDataParser :: Parser (PktID, SeqControl)
ackDataParser = do
    i <- PktID <$> A.anyWord16be
    s <- SeqControl <$> A.anyWord16be
    return (i, s)


processAsciiAck
    :: (MonadIO m, MonadReader env m, HasVerif env, HasLogFunc env)
    => ProtocolPacket PUSPacket
    -> m ()
processAsciiAck protPkt = do
    let hdr   = protPkt ^. protContent . pusHdr
        -- the ack is a TM packet, but the PktID to be used for verification is a 
        -- TC, therefore, we need to set the type flag in the PktID
        pktID = pktIdSetType (hdr ^. pusHdrPktID) PUSTC
        seqC  = hdr ^. pusHdrSeqCtrl
        ack   = BS8.pack "ACK"
        nak   = BS8.pack "NAK"
        dat   = BS.take 3 (toBS (protPkt ^. protContent . pusData))

    env <- ask
    if
        | dat == ack
        -> do
            liftIO $ requestVerifyGTCnC env (pktID, seqC) StGSuccess
        | dat == nak
        -> do
            liftIO $ requestVerifyGTCnC env (pktID, seqC) StGSuccess
        | otherwise
        -> logWarn $ "Could not parse C&C ACK data from pkt: " <> displayShow
            (protPkt ^. protContent)






cncToTMPacket
    :: (MonadIO m, MonadReader env m, HasLogFunc env)
    => ProtocolInterface
    -> Maybe SSC
    -> ConduitT
           (ByteString, ProtocolPacket PUSPacket)
           ExtractedPacket
           m
           ()
cncToTMPacket interf lastSSC = do
    x <- await
    case x of
        Nothing  -> return ()
        Just res -> do
            (newPkt, newSSC) <- convertCncToTMPacket res interf lastSSC
            logDebug $ "CnC TM: received packet: " <> displayShow newPkt
            yield newPkt
            cncToTMPacket interf (Just newSSC)


convertCncToTMPacket
    :: MonadIO m
    => (ByteString, ProtocolPacket PUSPacket)
    -> ProtocolInterface
    -> Maybe SSC
    -> m (ExtractedPacket, SSC)
convertCncToTMPacket (byts, protPkt) interf lastSSC = do
    ert <- liftIO getCurrentTime
    let epd = ExtractedDU { _epQuality = toFlag Good True
                          , _epERT     = ert
                          , _epGap     = determineGap lastSSC newSSC
                          , _epSource  = interf
                          , _epVCID    = IsSCOE
                          , _epDU      = pkt
                          }
        newSSC = pkt ^. pusHdr . pusHdrSSC
        pkt    = protPkt ^. protContent
        newPkt = ExtractedPacket byts epd

    return (newPkt, newSSC)
  where
    determineGap Nothing       _   = Nothing
    determineGap (Just oldSSC) ssc = if oldSSC + 1 == ssc
        then Nothing
        else Just (fromIntegral oldSSC, fromIntegral ssc)



sendTCCncC
    :: (MonadIO m, MonadReader env m, HasLogFunc env, HasVerif env)
    => ConduitT EncodedPUSPacket ByteString m ()
sendTCCncC = awaitForever $ \encPkt -> do
    case encPkt ^. encPktEncoded of
        Just (pkt, _pktId, _ssc) -> do
            logDebug $ display @Text "Encoded C&C:\n" <> display (hexdumpBS pkt)
            now <- liftIO $ getCurrentTime
            yield pkt

            -- confirm release for the verifications
            env <- ask
            liftIO $ requestReleased
                env
                (encPkt ^. encPktRequest . tcReqRequestID)
                now
                StRSuccess
        Nothing -> return ()


scoeCommandC
    :: (Monad m) => ConduitT (ProtocolPacket PUSPacket) SCOECommand m ()
scoeCommandC = awaitForever $ \pusPkt -> do
    maybe scoeCommandC yield (extractCommand pusPkt)


{-# INLINABLE extractCommand #-}
extractCommand :: ProtocolPacket PUSPacket -> Maybe SCOECommand
extractCommand cpkt@(ProtocolPacket (IfCnc _) pusPkt) =
    if isASCIICc cpkt then parseCncContent (toBS (_pusData pusPkt)) else Nothing
extractCommand _ = Nothing

{-# INLINABLE parseCncContent #-}
parseCncContent :: BCS.ByteString -> Maybe SCOECommand
parseCncContent cont = case parse scoeCommandParser cont of
    Fail{}    -> Nothing
    Partial c -> case c BCS.empty of
        Fail{}     -> Nothing
        Partial _  -> Nothing
        Done _ res -> Just res
    Done _ res -> Just res


data SCOECommand = SCOECommand
    { sccCommand :: !BS.ByteString
    , sccArgs    :: V.Vector BS.ByteString
    }
    deriving (Eq, Show, Read)

{-# INLINABLE scoeCommandParser #-}
scoeCommandParser :: Parser SCOECommand
scoeCommandParser = do
    cmd  <- commandParser
    args <- option V.empty $ A.try $ do
        void space
        V.fromList <$> argumentListParser
    return $! SCOECommand cmd args

{-# INLINABLE commandParser #-}
commandParser :: Parser BS.ByteString
commandParser = do
    (bs, _) <- A.match $ do
        void A.letter_ascii
        void $ A.takeWhile (inClass keywordChars)
    return (BCS.map toUpper bs)

keywordChars :: String
keywordChars = "_0..9a-zA-Z"

{-# INLINABLE argumentListParser #-}
argumentListParser :: Parser [BS.ByteString]
argumentListParser = argumentParser `A.sepBy` choice [space, A.char ',']

{-# INLINABLE argumentParser #-}
argumentParser :: Parser BS.ByteString
argumentParser = do
    A.takeWhile (\c -> c /= ' ' && c /= ',')


  -- TODO: for now we have a shortcut to quickly get out strings, but not parse completely
  --       This should be more done on SCOE side anyways.

  -- choice
  --   [ parameterAssignListParser
  --   , parameterAssignmentParser
  --   , valueParser
  --   , commandOptionParser
  --   ]

-- commandOptionParser :: Parser BS.ByteString
-- commandOptionParser = do
--   (bs, _) <- match $ do
--     void A.letter_ascii
--     void $ A.takeWhile (inClass idChars)
--   return bs

-- idChars :: String
-- idChars = "_-.a-zA-Z0-9"

-- parameterAssignmentParser :: Parser BS.ByteString
-- parameterAssignmentParser = do
--   (bs, _) <- match $ do
--     void parameterIdentifier
--     void $ option Nothing (Just <$> listItemParser)
--     void $ A.char '='
--     void valueParser
--   return bs

-- parameterIdentifier :: Parser BS.ByteString
-- parameterIdentifier = do
--   (bs, _) <- match $ do
--     void A.letter_ascii
--     void $ A.takeWhile (inClass keywordChars)
--   return bs

-- listItemParser :: Parser Int
-- listItemParser = do
--   void $ A.char '('
--   v <- A.decimal
--   void $ A.char ')'
--   return v

-- valueParser :: Parser BS.ByteString
-- valueParser = do
--   (bs, _) <- match $ choice [characterString, identifier, numericLiteral]
--   return bs
--  where
--   numericLiteral = do
--     (bs, _) <- match $ A.try hexNumber <|> decimalNumber
--     return bs
--   hexNumber = do
--     (bs, _) <- match $ do
--       void $ A.char '0'
--       void $ A.try (A.char 'x') <|> A.char 'X'
--       A.hexadecimal :: Parser Int
--     return bs
--   decimalNumber   = fst <$> match double

--   characterString = do
--     void $ A.char '"'
--     v <- A.takeWhile (/= '"')
--     void $ A.char '"'
--     return v

--   identifier = do
--     (bs, _) <- match $ do
--       void A.letter_ascii
--       void $ A.takeWhile (inClass keywordChars)
--     return bs

-- parameterAssignListParser :: Parser BS.ByteString
-- parameterAssignListParser = do
--   (bs, _) <- match $ do
--     void parameterAssignmentParser
--     many1 $ do
--       void $ A.char ','
--       valueParser
--   return bs


{-# INLINABLE generateAckData #-}
generateAckData :: SCOECommand -> Maybe ByteString -> ByteString
generateAckData (SCOECommand "TRANSFER" args) Nothing =
    builderBytes $ bytes "ACK " <> bytes "TRANSFER " <> bytes (args V.! 0)
generateAckData (SCOECommand "TRANSFER" args) (Just reason) =
    builderBytes
        $  bytes "NAK "
        <> bytes "TRANSFER "
        <> bytes (args V.! 0)
        <> bytes " "
        <> bytes reason

generateAckData (SCOECommand "ONLINE" _args) Nothing =
    builderBytes $ bytes "ACK " <> bytes "ONLINE "
generateAckData (SCOECommand "ONLINE" _args) (Just reason) =
    builderBytes $ bytes "NAK " <> bytes "ONLINE " <> bytes reason

generateAckData (SCOECommand "OFFLINE" _args) Nothing =
    builderBytes $ bytes "ACK " <> bytes "OFFLINE "
generateAckData (SCOECommand "OFFLINE" _args) (Just reason) =
    builderBytes $ bytes "NAK " <> bytes "OFFLINE " <> bytes reason

generateAckData (SCOECommand "RESET" _args) Nothing = "ACK " <> "RESET "
generateAckData (SCOECommand "RESET" _args) (Just reason) =
    "NAK " <> "RESET " <> " " <> reason

generateAckData (SCOECommand "RESTART" args) Nothing =
    "ACK " <> "RESTART " <> args V.! 0
generateAckData (SCOECommand "RESTART" args) (Just reason) =
    "NAK " <> "RESTART " <> args V.! 0 <> " " <> reason

generateAckData (SCOECommand "START" args) Nothing =
    "ACK " <> "START " <> args V.! 0
generateAckData (SCOECommand "START" args) (Just reason) =
    "NAK " <> "START " <> args V.! 0 <> " " <> reason

generateAckData (SCOECommand "STOP" args) Nothing =
    "ACK " <> "STOP " <> args V.! 0
generateAckData (SCOECommand "STOP" args) (Just reason) =
    "NAK " <> "STOP " <> args V.! 0 <> " " <> reason

generateAckData (SCOECommand "APPLY" args) Nothing =
    "ACK " <> "APPLY " <> args V.! 0
generateAckData (SCOECommand "APPLY" args) (Just reason) =
    "NAK " <> "APPLY " <> args V.! 0 <> " " <> reason

generateAckData (SCOECommand "SET" args) Nothing =
    "ACK " <> "SET " <> args V.! 0
generateAckData (SCOECommand "SET" args) (Just reason) =
    "NAK " <> "SET " <> args V.! 0 <> " " <> reason

generateAckData (SCOECommand "REPORTPARA" args) Nothing =
    "ACK " <> "REPORTPARA " <> args V.! 0
generateAckData (SCOECommand "REPORTPARA" args) (Just reason) =
    "NAK " <> "REPORTPARA " <> args V.! 0 <> " " <> reason

generateAckData (SCOECommand "TEST" args) Nothing =
    "ACK " <> "TEST " <> args V.! 0
generateAckData (SCOECommand "TEST" args) (Just reason) =
    "NAK " <> "TEST " <> args V.! 0 <> " " <> reason

generateAckData (SCOECommand "REPORT" args) Nothing =
    "ACK " <> "REPORT " <> args V.! 0
generateAckData (SCOECommand "REPORT" args) (Just reason) =
    "NAK " <> "REPORT " <> args V.! 0 <> " " <> reason

generateAckData (SCOECommand "GETTM" args) Nothing =
    "ACK " <> "GETTM " <> args V.! 0
generateAckData (SCOECommand "GETTM" args) (Just reason) =
    "NAK " <> "GETTM " <> args V.! 0 <> " " <> reason


generateAckData (SCOECommand x args) Nothing =
    "ACK " <> x <> " " <> if V.length args >= 1 then args V.! 0 else BS.empty
generateAckData (SCOECommand x args) (Just reason) =
    "ACK " <> x <> " " <> arg1 <> " " <> reason
    where arg1 = if V.length args >= 1 then args V.! 0 else BS.empty
