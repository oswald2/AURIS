{-# LANGUAGE 
    OverloadedStrings
    , BangPatterns
    , MultiWayIf
    , NoImplicitPrelude
    , TemplateHaskell 
    , TypeApplications
    , LambdaCase
#-}
module Protocol.EDEN
    ( EdenMessage(..)
    , EdenData(..)
    , EdenTCType(..)
    , EdenTCOrigin(..)
    , EdenTCSequenceFlags(..)
    , EdenTcSecHeader(..)
    , EdenTcSecSCOEHeader(..)
    , EdenTmSecHeader(..)
    , EdenType(..)
    , EdenSubType(..)
    , receiveEdenMessageC
    , encodeEdenMessageC
    , edenHdrLen
    , edenMessageBuilder
    , edenSecStructure
    , edenSecChannel
    , edenSecTCType
    , edenSecTCID
    , edenSecTCOrigin
    , edenSecTime
    , edenSecMapID
    , edenSecTCEchoStatus
    , edenSecSequenceFlags
    , edenSecScoeStructure
    , edenSecScoeTCID
    , edenSecScoeTCOrigin
    , edenSecScoeTime
    , edenSecScoeTCEchoStatus
    , edenTmStructure
    , edenTmSecChannel
    , edenTmSecDataQuality
    , edenTmSecCLCW
    , edenTmSecTime
    , edenTmSecMCFC
    , edenTmSecVCFC
    , edenTmScoeStructure
    , edenTmSecScoeTime
    , edenTmSecScoeHeader
    , edenTmScoeData
    , edenRawData
    , edenSpaceSecHeader
    , edenSpaceData
    , edenSCOESecHeader
    , edenScoeData
    , edenTmSecHeader
    , edenTmData
    , edenType
    , edenSubType
    , edenField1
    , edenField2
    , edenField3
    , edenDataFieldLength
    , edenDataField
    , edenEmptyField1
    , edenRqstID
    , getEdenPacketData
    , getEdenRawData
    ) where

import           RIO                     hiding ( Builder )
import qualified RIO.Text                      as T

import           Control.Lens                   ( makeLenses )

import qualified Data.ByteString               as B
import qualified Data.ByteString.Char8         as C
import           Data.Char


import           Control.PUS.Classes

import           ByteString.StrictBuilder
import qualified Data.Attoparsec.Binary        as A
import           Data.Attoparsec.ByteString     ( Parser )
import qualified Data.Attoparsec.ByteString    as A

import           Data.Conduit
import           Data.Conduit.Attoparsec

import           Data.PUS.CLCW
import           Data.PUS.Events
import           Data.PUS.Verification

import           General.SizeOf

import           General.Hexdump
import           General.PUSTypes               ( RequestID
                                                , mkRqstID
                                                )
import           General.Padding
import           General.Time
import           General.Types
import           Text.Show.Pretty


data EdenType =
    EdenTCType
    | EdenTCAType
    | EdenTCEType
    | EdenTMType
    | EdenTMDType
    | EdenTCDType
    | EdenUserType
    | EdenErrorType
    | EdenDumpType
    | EdenCmdType
    | EdenSeqType
    deriving (Eq, Ord, Enum, Show, Read, Generic)


data EdenSubType =
    EdenSpace
    | EdenSCOE
    | EdenSpecifOnd
    | EdenStop
    | EdenStatus
    | EdenProtocol
    | EdenUnknown
    | EdenTimeout
    | EdenFrame
    | EdenEnvelope
    | EdenExec
    | EdenAnsw
    | EdenLog
    | EdenLogErr
    | EdenSpare
    deriving (Eq, Ord, Enum, Show, Read, Generic)

data EdenTCType = EdenTcPacket
    | EdenTcSegment
    | EdenTcFrame
    | EdenTcCltu
    | EdenTcPhysical
    deriving (Ord, Eq, Enum, Show, Read, Generic)

data EdenTCOrigin = EdenOriginLocal
    | EdenOriginCCS
    | EdenOriginOCC
    | EdenOriginOCCNDIU
    | EdenOriginPlayback
    deriving (Ord, Eq, Enum, Show, Read, Generic)

data EdenTCSequenceFlags = EdenSegContinuation
    | EdenSegFirst
    | EdenSegLast
    | EdenSegUnsegmented
    deriving (Ord, Eq, Enum, Show, Read, Generic)




data EdenTcSecHeader = EdenTcSecHeader
    { _edenSecStructure     :: !Word8
    , _edenSecChannel       :: !Word8
    , _edenSecTCType        :: !EdenTCType
    , _edenSecTCID          :: !Word32
    , _edenSecTCOrigin      :: !EdenTCOrigin
    , _edenSecTime          :: !C.ByteString
    , _edenSecMapID         :: !Word8
    , _edenSecTCEchoStatus  :: !Word8
    , _edenSecSequenceFlags :: !EdenTCSequenceFlags
    }
    deriving (Read, Show, Generic)
makeLenses ''EdenTcSecHeader


data EdenTcSecSCOEHeader = EdenTcSecSCOEHeader
    { _edenSecScoeStructure    :: !Word8
    , _edenSecScoeTCID         :: !Word32
    , _edenSecScoeTCOrigin     :: !EdenTCOrigin
    , _edenSecScoeTime         :: !C.ByteString
    , _edenSecScoeTCEchoStatus :: !Word8
    }
    deriving (Read, Show, Generic)
makeLenses ''EdenTcSecSCOEHeader


data EdenTmStructureType =
  EdenTmSpace
  | EdenTmSCOE
  | EdenTmUnknown
  deriving (Eq, Ord, Enum, Read, Show, Generic)


data EdenTmSecHeader = EdenTmSecHeader
    { _edenTmStructure      :: !EdenTmStructureType
    , _edenTmSecChannel     :: !Word8
    , _edenTmSecDataQuality :: !Word8
    , _edenTmSecCLCW        :: !CLCW
    , _edenTmSecTime        :: !C.ByteString
    , _edenTmSecMCFC        :: !Word8
    , _edenTmSecVCFC        :: !Word8
    }
    deriving (Read, Show, Generic)
makeLenses ''EdenTmSecHeader

data EdenTmSecSCOEHeader = EdenTmSecSCOEHeader
    { _edenTmScoeStructure :: !EdenTmStructureType
    , _edenTmSecScoeTime   :: !C.ByteString
    }
    deriving (Read, Show, Generic)
makeLenses ''EdenTmSecSCOEHeader


data EdenData =
    EdenRawData {
        _edenRawData :: HexBytes
    }
    | EdenSpaceTC {
        _edenSpaceSecHeader :: EdenTcSecHeader,
        _edenSpaceData :: HexBytes
    }
    | EdenSCOETC {
        _edenSCOESecHeader :: EdenTcSecSCOEHeader,
        _edenScoeData :: HexBytes
    }
    | EdenTM {
        _edenTmSecHeader :: EdenTmSecHeader,
        _edenTmData :: HexBytes
    }
    | EdenSCOETM {
        _edenTmSecScoeHeader :: EdenTmSecSCOEHeader,
        _edenTmScoeData :: HexBytes
    } deriving (Show, Read)
makeLenses ''EdenData


getEdenPacketData :: EdenData -> Maybe HexBytes
getEdenPacketData EdenSpaceTC {..} = Just _edenSpaceData
getEdenPacketData EdenSCOETC {..}  = Just _edenScoeData
getEdenPacketData EdenTM {..}      = Just _edenTmData
getEdenPacketData EdenSCOETM {..}  = Just _edenTmScoeData
getEdenPacketData EdenRawData{}    = Nothing

getEdenRawData :: EdenData -> HexBytes
getEdenRawData EdenSpaceTC {..} = _edenSpaceData
getEdenRawData EdenSCOETC {..}  = _edenScoeData
getEdenRawData EdenTM {..}      = _edenTmData
getEdenRawData EdenSCOETM {..}  = _edenTmScoeData
getEdenRawData EdenRawData {..} = _edenRawData



-- | Eden Message structure used for the EDEN protocol
data EdenMessage = EdenMessage
    { _edenType            :: !EdenType
    , _edenSubType         :: !EdenSubType
    , _edenField1          :: !C.ByteString
    , _edenField2          :: !Word32
    , _edenField3          :: !Word32
    , _edenDataFieldLength :: !Word32
    , _edenDataField       :: EdenData
    }
    deriving (Show, Read)
makeLenses ''EdenMessage


edenEmptyField1 :: C.ByteString
edenEmptyField1 = C.replicate 16 ' '


edenRqstID :: EdenMessage -> Maybe RequestID
edenRqstID EdenMessage {..} = rqstID _edenDataField
  where
    rqstID EdenSpaceTC {..} =
        Just (mkRqstID (_edenSpaceSecHeader ^. edenSecTCID))
    rqstID EdenSCOETC {..} =
        Just (mkRqstID (_edenSCOESecHeader ^. edenSecScoeTCID))
    rqstID _ = Nothing



instance FixedSize EdenTcSecHeader where
    fixedSizeOf = 36

instance FixedSize EdenTcSecSCOEHeader where
    fixedSizeOf = 36

instance FixedSize EdenTmSecHeader where
    fixedSizeOf = 36

instance FixedSize EdenTmSecSCOEHeader where
    fixedSizeOf = 36


instance SizeOf EdenData where
    sizeof (EdenRawData x     ) = hexLength x
    sizeof (EdenSpaceTC _hdr x) = fixedSizeOf @EdenTcSecHeader + hexLength x
    sizeof (EdenSCOETC  _hdr x) = fixedSizeOf @EdenTcSecSCOEHeader + hexLength x
    sizeof (EdenTM      _hdr x) = fixedSizeOf @EdenTmSecHeader + hexLength x
    sizeof (EdenSCOETM  _hdr x) = fixedSizeOf @EdenTmSecSCOEHeader + hexLength x


edenHdrLen :: Int
edenHdrLen = 42

-- | instance declaration for Show
instance Display EdenMessage where
    display x =
        "EdenMessage:\n  Type      : "
            <> displayShow (_edenType x)
            <> "\n  SubType   : "
            <> displayShow (_edenSubType x)
            <> "\n  Field1    : "
            <> displayBytesUtf8 (_edenField1 x)
            <> "\n  Field2    : "
            <> displayShow (_edenField2 x)
            <> "\n  Field3    : "
            <> displayShow (_edenField3 x)
            <> "\n  DataLength: "
            <> displayShow (_edenDataFieldLength x)
            <> "\n  Data      :\n"
            <> fromString (ppShow (_edenDataField x))

instance Display EdenData where
    display (EdenRawData (HexBytes x)) = "Raw Data: " <> displayBytesUtf8 x
    display (EdenSpaceTC hdr (HexBytes x)) =
        "Space TC: " <> display hdr <> "\n  Data:\n" <> display (hexdumpBS x)
    display (EdenSCOETC hdr (HexBytes x)) =
        "SCOE TC: " <> display hdr <> "\n  Data:\n" <> display (hexdumpBS x)
    display (EdenTM hdr (HexBytes x)) =
        "TM: " <> display hdr <> "\n  Data:\n" <> display (hexdumpBS x)
    display (EdenSCOETM hdr (HexBytes x)) =
        "TM SCOE: " <> display hdr <> "\n  Data:\n" <> display (hexdumpBS x)



instance Display EdenTmSecHeader where
    display x =
        "TM Secondary Header:\n  Virtual Channel   : "
            <> displayShow (_edenTmSecChannel x)
            <> "\n  Data Quality      : "
            <> displayShow (_edenTmSecDataQuality x)
            <> "\n  CLCW              : "
            <> displayShow (_edenTmSecCLCW x)
            <> "\n  Time              : "
            <> displayBytesUtf8 (_edenTmSecTime x)
            <> "\n  Master Channel FC : "
            <> displayShow (_edenTmSecMCFC x)
            <> "\n  Virtual Channel FC: "
            <> displayShow (_edenTmSecVCFC x)

instance Display EdenTmSecSCOEHeader where
    display x = "TM Secondary SCOE Header:\n  Time: "
        <> displayBytesUtf8 (_edenTmSecScoeTime x)


instance Display EdenTcSecHeader where
    display x =
        "TC Secondary Header:\n  Structure      : "
            <> displayShow (_edenSecStructure x)
            <> "\n  Virtual Channel: "
            <> displayShow (_edenSecChannel x)
            <> "\n  TC Type        : "
            <> displayShow (_edenSecTCType x)
            <> "\n  TC ID          : "
            <> displayShow (_edenSecTCID x)
            <> "\n  TC Origin      : "
            <> displayShow (_edenSecTCOrigin x)
            <> "\n  Time           : "
            <> displayBytesUtf8 (_edenSecTime x)
            <> "\n  MAP ID         : "
            <> displayShow (_edenSecMapID x)
            <> "\n  Echo Status    : "
            <> displayShow (_edenSecTCEchoStatus x)
            <> "\n  Sequence Flags : "
            <> displayShow (_edenSecSequenceFlags x)

instance Display EdenTcSecSCOEHeader where
    display x =
        "TC SCOE Secondary Header:\n  Structure: "
            <> displayShow (_edenSecScoeStructure x)
            <> "\n  TC ID: "
            <> displayShow (_edenSecScoeTCID x)
            <> "\n  TC Origin: "
            <> displayShow (_edenSecScoeTCOrigin x)
            <> "\n  Time: "
            <> displayBytesUtf8 (_edenSecScoeTime x)
            <> "\n  Echo Status: "
            <> displayShow (_edenSecScoeTCEchoStatus x)




formatField :: Int -> C.ByteString -> C.ByteString
formatField width val =
    rightPaddedC ' ' width $ C.map toUpper $ C.take width val



edenTcTypeBuilder :: EdenTCType -> Builder
edenTcTypeBuilder EdenTcPacket   = word8 0
edenTcTypeBuilder EdenTcSegment  = word8 1
edenTcTypeBuilder EdenTcFrame    = word8 2
edenTcTypeBuilder EdenTcCltu     = word8 3
edenTcTypeBuilder EdenTcPhysical = word8 4


edenTcOriginBuilder :: EdenTCOrigin -> Builder
edenTcOriginBuilder EdenOriginLocal    = word8 0
edenTcOriginBuilder EdenOriginCCS      = word8 1
edenTcOriginBuilder EdenOriginOCC      = word8 2
edenTcOriginBuilder EdenOriginOCCNDIU  = word8 3
edenTcOriginBuilder EdenOriginPlayback = word8 4



edenTCSequenceFlagsBuilder :: EdenTCSequenceFlags -> Builder
edenTCSequenceFlagsBuilder EdenSegContinuation = word8 0
edenTCSequenceFlagsBuilder EdenSegFirst        = word8 1
edenTCSequenceFlagsBuilder EdenSegLast         = word8 2
edenTCSequenceFlagsBuilder EdenSegUnsegmented  = word8 3



edenTcSecHeaderBuilder :: EdenTcSecHeader -> Builder
edenTcSecHeaderBuilder x =
    word8 (_edenSecStructure x)
        <> word8 (_edenSecChannel x)
        <> word8 0
        <> edenTcTypeBuilder (_edenSecTCType x)
        <> word32BE (_edenSecTCID x)
        <> edenTcOriginBuilder (_edenSecTCOrigin x)
        <> word8 255
        <> bytes (rightPaddedC ' ' 22 (_edenSecTime x))
        <> word8 (_edenSecMapID x)
        <> word8 0
        <> word8 (_edenSecTCEchoStatus x)
        <> edenTCSequenceFlagsBuilder (_edenSecSequenceFlags x)



edenTcSecSCOEHeaderBuilder :: EdenTcSecSCOEHeader -> Builder
edenTcSecSCOEHeaderBuilder x =
    word8 (_edenSecScoeStructure x)
        <> word8 0
        <> word8 0
        <> word8 0
        <> word32BE (_edenSecScoeTCID x)
        <> edenTcOriginBuilder (_edenSecScoeTCOrigin x)
        <> word8 255
        <> bytes (rightPaddedC ' ' 22 (_edenSecScoeTime x))
        <> word8 0
        <> word8 0
        <> word8 (_edenSecScoeTCEchoStatus x)
        <> word8 0


edenTmStructureBuilder :: EdenTmStructureType -> Builder
edenTmStructureBuilder EdenTmSpace   = word8 1
edenTmStructureBuilder EdenTmSCOE    = word8 3
edenTmStructureBuilder EdenTmUnknown = word8 0


edenTmSecHeaderBuilder :: EdenTmSecHeader -> Builder
edenTmSecHeaderBuilder x =
    edenTmStructureBuilder (_edenTmStructure x)
        <> word8 (_edenTmSecChannel x)
        <> word8 (_edenTmSecDataQuality x)
        <> word8 0
        <> clcwBuilder (_edenTmSecCLCW x)
        <> word16BE 0
        <> bytes (_edenTmSecTime x)
        <> word8 (_edenTmSecMCFC x)
        <> word8 (_edenTmSecVCFC x)
        <> word16BE 0

edenTmSecScoeHeaderBuilder :: EdenTmSecSCOEHeader -> Builder
edenTmSecScoeHeaderBuilder x =
    edenTmStructureBuilder (_edenTmScoeStructure x)
        <> bytes filler
        <> bytes (_edenTmSecScoeTime x)
        <> word32BE 0
    where filler = B.replicate 9 0


edenTypeBuilder :: EdenType -> Builder
edenTypeBuilder EdenTCType    = bytes "TC  "
edenTypeBuilder EdenTCAType   = bytes "TC-A"
edenTypeBuilder EdenTCEType   = bytes "TC-E"
edenTypeBuilder EdenTMType    = bytes "TM  "
edenTypeBuilder EdenTMDType   = bytes "TM-D"
edenTypeBuilder EdenTCDType   = bytes "TC-D"
edenTypeBuilder EdenUserType  = bytes "USER"
edenTypeBuilder EdenErrorType = bytes "ERR "
edenTypeBuilder EdenDumpType  = bytes "DUMP"
edenTypeBuilder EdenCmdType   = bytes "CMD "
edenTypeBuilder EdenSeqType   = bytes "SEQ "


edenTypeParser :: Parser EdenType
edenTypeParser = do
    st <- A.take 4
    case st of
        "TC  " -> return EdenTCType
        "TC-A" -> return EdenTCAType
        "TC-E" -> return EdenTCEType
        "TM  " -> return EdenTMType
        "TM-D" -> return EdenTMDType
        "TC-D" -> return EdenTCDType
        "USER" -> return EdenUserType
        "ERR " -> return EdenErrorType
        "DUMP" -> return EdenDumpType
        "CMD " -> return EdenCmdType
        "SEQ " -> return EdenSeqType
        _      -> return EdenErrorType


subTypeWidth :: Int
subTypeWidth = 10



edenSubTypeBuilder :: EdenSubType -> Builder
edenSubTypeBuilder EdenSpace = bytes (formatField subTypeWidth "SPACE")
edenSubTypeBuilder EdenSCOE  = bytes (formatField subTypeWidth "SCOE")
edenSubTypeBuilder EdenSpecifOnd =
    bytes (formatField subTypeWidth "SPECIF_OND")
edenSubTypeBuilder EdenStop     = bytes (formatField subTypeWidth "STOP")
edenSubTypeBuilder EdenStatus   = bytes (formatField subTypeWidth "STATUS")
edenSubTypeBuilder EdenProtocol = bytes (formatField subTypeWidth "PROTOCOL")
edenSubTypeBuilder EdenUnknown  = bytes (formatField subTypeWidth "UNKNOWN")
edenSubTypeBuilder EdenTimeout  = bytes (formatField subTypeWidth "TIMEOUT")
edenSubTypeBuilder EdenFrame    = bytes (formatField subTypeWidth "FRAME")
edenSubTypeBuilder EdenEnvelope = bytes (formatField subTypeWidth "ENVELOPE")
edenSubTypeBuilder EdenExec     = bytes (formatField subTypeWidth "EXEC")
edenSubTypeBuilder EdenAnsw     = bytes (formatField subTypeWidth "ANSW")
edenSubTypeBuilder EdenLog      = bytes (formatField subTypeWidth "LOG")
edenSubTypeBuilder EdenLogErr   = bytes (formatField subTypeWidth "ERR")
edenSubTypeBuilder EdenSpare    = bytes (formatField subTypeWidth "")


edenSubTypeParser :: Parser EdenSubType
edenSubTypeParser = do
    st <- A.take subTypeWidth
    if
        | "SPACE" `B.isPrefixOf` st      -> return EdenSpace
        | "SCOE" `B.isPrefixOf` st       -> return EdenSCOE
        | "SPECIF_OND" `B.isPrefixOf` st -> return EdenSpecifOnd
        | "STOP" `B.isPrefixOf` st       -> return EdenStop
        | "STATUS" `B.isPrefixOf` st     -> return EdenStatus
        | "PROTOCOL" `B.isPrefixOf` st   -> return EdenProtocol
        | "UNKNOWN" `B.isPrefixOf` st    -> return EdenUnknown
        | "TIMEOUT" `B.isPrefixOf` st    -> return EdenTimeout
        | "FRAME" `B.isPrefixOf` st      -> return EdenFrame
        | "ENVELOPE" `B.isPrefixOf` st   -> return EdenEnvelope
        | "EXEC" `B.isPrefixOf` st       -> return EdenExec
        | "ANSW" `B.isPrefixOf` st       -> return EdenAnsw
        | "LOG" `B.isPrefixOf` st        -> return EdenLog
        | "ERR" `B.isPrefixOf` st        -> return EdenLogErr
        | otherwise                      -> return EdenSpare







receiveEdenMessageC
    :: (MonadIO m, MonadReader env m, HasGlobalState env)
    => ConduitT ByteString EdenMessage m ()
receiveEdenMessageC = conduitParserEither edenMessageParser .| sink
  where
    sink = awaitForever $ \case
        Left err -> do
            raiseEvent $ EVAlarms (EVEDENParseError (T.pack (errorMessage err)))
        Right (_, tc) -> do
            logDebug $ "receiveEdenMessageC: received: " <> display tc
            yield tc



encodeEdenMessageC
    :: (MonadIO m, MonadReader env m, HasLogFunc env, HasVerif env)
    => ConduitT EdenMessage ByteString m ()
encodeEdenMessageC = awaitForever $ \du -> do
    let enc = builderBytes (edenMessageBuilder du)
    logDebug $ "Encoded EDEN: " <> fromString (ppShow du) <> ":\n" <> display
        (hexdumpBS enc)
    yield enc

    -- notify that we have released the request
    env <- ask
    liftIO $ do
        case edenRqstID du of
            Just rqstID -> do
                now <- getCurrentTime
                requestReleased env rqstID now StRSuccess
            Nothing -> return ()


edenMessageParser :: Parser EdenMessage
edenMessageParser = do
    t   <- edenTypeParser
    st  <- edenSubTypeParser
    f1  <- A.take 16
    f2  <- A.anyWord32be
    f3  <- A.anyWord32be
    len <- A.anyWord32be

    dat <- if
        | t == EdenTCType || t == EdenTCEType -> case st of
            EdenSpace -> edenSpaceDataParser (fromIntegral len)
            EdenSCOE  -> edenScoeDataParser (fromIntegral len)
            _         -> edenRawDataParser (fromIntegral len)
        | t == EdenTCAType -> edenRawDataParser (fromIntegral len)
        | t == EdenTMDType -> if st == EdenSCOE
            then edenScoeDataParser (fromIntegral len)
            else edenRawDataParser (fromIntegral len)
        | t == EdenTMType -> case st of
            EdenSpace -> edenTmDataParser (fromIntegral len)
            EdenSCOE  -> edenTmScoeDataParser (fromIntegral len)
            _         -> edenRawDataParser (fromIntegral len)
        | t == EdenTMDType -> if st == EdenSCOE
            then edenTmDataParser (fromIntegral len)
            else edenRawDataParser (fromIntegral len)
        | otherwise -> edenRawDataParser (fromIntegral len)
    return $ EdenMessage t st f1 f2 f3 len dat


edenMessageBuilder :: EdenMessage -> Builder
edenMessageBuilder x =
    edenTypeBuilder (_edenType x)
        <> edenSubTypeBuilder (_edenSubType x)
        <> bytes (formatField 16 (_edenField1 x))
        <> word32BE (_edenField2 x)
        <> word32BE (_edenField3 x)
        <> word32BE (fromIntegral (sizeof dataField))
        <> edenDataBuilder dataField
    where dataField = _edenDataField x

edenDataBuilder :: EdenData -> Builder
edenDataBuilder (EdenRawData (HexBytes x)) = bytes x
edenDataBuilder (EdenSpaceTC hdr (HexBytes x)) =
    edenTcSecHeaderBuilder hdr <> bytes x
edenDataBuilder (EdenSCOETC hdr (HexBytes x)) =
    edenTcSecSCOEHeaderBuilder hdr <> bytes x
edenDataBuilder (EdenTM hdr (HexBytes x)) =
    edenTmSecHeaderBuilder hdr <> bytes x
edenDataBuilder (EdenSCOETM hdr (HexBytes x)) =
    edenTmSecScoeHeaderBuilder hdr <> bytes x


edenRawDataParser :: Int -> Parser EdenData
edenRawDataParser len = EdenRawData . HexBytes <$> A.take len

edenSpaceDataParser :: Int -> Parser EdenData
edenSpaceDataParser len =
    EdenSpaceTC
        <$> edenSpaceSecHeaderParser
        <*> (HexBytes <$> A.take (len - fixedSizeOf @EdenTcSecHeader))

edenScoeDataParser :: Int -> Parser EdenData
edenScoeDataParser len =
    EdenSCOETC
        <$> edenScoeSecHeaderParser
        <*> (HexBytes <$> A.take (len - fixedSizeOf @EdenTcSecSCOEHeader))

edenTmDataParser :: Int -> Parser EdenData
edenTmDataParser len =
    EdenTM
        <$> edenTmSecHeaderParser
        <*> (HexBytes <$> A.take (len - fixedSizeOf @EdenTmSecHeader))

edenTmScoeDataParser :: Int -> Parser EdenData
edenTmScoeDataParser len =
    EdenSCOETM
        <$> edenTmSecScoeHeaderParser
        <*> (HexBytes <$> A.take (len - fixedSizeOf @EdenTmSecSCOEHeader))


edenSpaceSecHeaderParser :: Parser EdenTcSecHeader
edenSpaceSecHeaderParser = do
    str    <- A.anyWord8
    ch     <- A.anyWord8
    _      <- A.anyWord8
    tctype <- edenTCTypeParser
    tcid   <- A.anyWord32be
    tcorig <- edenTCOriginParser
    _      <- A.anyWord8
    tim    <- A.take 22
    mapid  <- A.anyWord8
    _      <- A.anyWord8
    e      <- A.anyWord8
    EdenTcSecHeader str ch tctype tcid tcorig tim mapid e
        <$> edenTCSequenceFlagsParser



edenScoeSecHeaderParser :: Parser EdenTcSecSCOEHeader
edenScoeSecHeaderParser = do
    str    <- A.anyWord8
    _      <- A.take 3
    tcid   <- A.anyWord32be
    tcorig <- edenTCOriginParser
    _      <- A.anyWord8
    tim    <- A.take 22
    _      <- A.take 2
    e      <- A.anyWord8
    _      <- edenTCSequenceFlagsParser
    return $ EdenTcSecSCOEHeader str tcid tcorig tim e

edenTmSecHeaderParser :: Parser EdenTmSecHeader
edenTmSecHeaderParser = do
    str  <- edenTmStructureTypeParser
    chan <- A.anyWord8
    qual <- A.anyWord8
    _    <- A.anyWord8
    clcw <- unpackValues <$> A.anyWord32be
    _    <- A.anyWord16be
    tim  <- A.take 22
    mc   <- A.anyWord8
    vc   <- A.anyWord8
    _    <- A.anyWord16be
    return $ EdenTmSecHeader str chan qual clcw tim mc vc

edenTmSecScoeHeaderParser :: Parser EdenTmSecSCOEHeader
edenTmSecScoeHeaderParser = do
    str <- edenTmStructureTypeParser
    _   <- A.take 9
    tim <- A.take 22
    _   <- A.take 4
    return $ EdenTmSecSCOEHeader str tim



edenTmStructureTypeParser :: Parser EdenTmStructureType
edenTmStructureTypeParser = do
    str <- A.anyWord8
    case str of
        1 -> return EdenTmSpace
        3 -> return EdenTmSCOE
        _ -> return EdenTmUnknown


edenTCTypeParser :: Parser EdenTCType
edenTCTypeParser = do
    v <- A.anyWord8
    case v of
        0 -> return EdenTcPacket
        1 -> return EdenTcSegment
        2 -> return EdenTcFrame
        3 -> return EdenTcCltu
        4 -> return EdenTcPhysical
        _ -> return EdenTcPacket

edenTCOriginParser :: Parser EdenTCOrigin
edenTCOriginParser = do
    v <- A.anyWord8
    case v of
        0 -> return EdenOriginLocal
        1 -> return EdenOriginCCS
        2 -> return EdenOriginOCC
        3 -> return EdenOriginOCCNDIU
        4 -> return EdenOriginPlayback
        _ -> return EdenOriginLocal



edenTCSequenceFlagsParser :: Parser EdenTCSequenceFlags
edenTCSequenceFlagsParser = do
    v <- A.anyWord8
    case v of
        0 -> return EdenSegContinuation
        1 -> return EdenSegFirst
        2 -> return EdenSegLast
        3 -> return EdenSegUnsegmented
        _ -> return EdenSegUnsegmented


