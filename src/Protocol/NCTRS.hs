{-# LANGUAGE OverloadedStrings
    , BangPatterns
    , GeneralizedNewtypeDeriving
    , DeriveGeneric
    , RecordWildCards
    , NoImplicitPrelude
    , TemplateHaskell
    , TypeApplications
#-}
module Protocol.NCTRS
    ( NcduHeaderType(..)
    , NcduTcHeader(..)
    , ncduPktSize
    , ncduType
    , ncduScID
    , NcduTcCltuHeader(..)
    , ncduTcCltuTransType
    , ncduTcCltuSeqCnt
    , ncduTcCltuVcID
    , ncduTcCltuMapID
    , ncduTcCltuAggrFlags
    , ncduTcCltuEarliestProdTimeFlags
    , ncduTcCltuEarliestProdTime
    , ncduTcCltuLatestProdTimeFlag
    , ncduTcCltuLatestProdTime
    , ncduTcCltuDelay
    , NcduTcPktHeader(..)
    , ncduTcPktTransType
    , ncduTcPktSeqCnt
    , ncduTcPktVcID
    , ncduTcPktMapID
    , NcduTmStreamType(..)
    , NcduTmDuHeader
    , ncduTmSize
    , ncduTmScID
    , ncduTmDataStreamType
    , ncduTmVcID
    , ncduTmGsID
    , ncduTmERT
    , ncduTmSeqFlag
    , ncduTmQualityFlag
    , NcduTmDu(..)
    , ncduTmHeader
    , ncduTmData
    , NcduTcCltuRespAck(..)
    , NcduTcCltuResponse(..)
    , ncduCltuRespTime
    , ncduCltuRespTransType
    , ncduCltuRespGsID
    , ncduCltuRespTCID
    , ncduCltuRespAck
    , ncduCltuRespReason
    , ncduCltuRespSpaceInQueue
    , ncduCltuRespNextTCID
    , ncduCltuLastCLCW
    , NcduAdminMessageType(..)
    , NcduAdminMessageSeverity(..)
    , NcduAdminMessage(..)
    , ncduAdmLength
    , ncduAdmTime
    , ncduAdmType
    , ncduAdmSeverity
    , ncduAdmEventID
    , ncduAdmMsg
    , NcduTcData(..)
    , ncduTcPktHdr
    , ncduTcPktData
    , ncduTcCltuHdr
    , ncduTcCltuData
    , ncduTcCltuResp
    , NcduTcDu(..)
    , ncduTcHdr
    , ncduTcData
    , receiveTcNcduC
    , receiveTmNcduC
    , receiveAdminNcduC
    , encodeTcNcduC
    , encodeTmNcduC
    , encodeAdminNcduC
    , ncduAdminMsgTcEstablished
    , ncduAdminMsgTcClosed
    , ncduAdminMsgTcAborted
    , ncduAdminMsgTcAbortedFromGS
    , ncduAdminMsgTcADavailable
    , ncduAdminMsgTmTMFLOW
    , ncduAdminMsgTmNOTMFLOW
    )
where


import           RIO                     hiding ( Builder )
import           Prelude                        ( toEnum )

import qualified RIO.ByteString                as B
import           RIO.List                       ( intersperse )
import qualified RIO.Text                      as T

import           Control.Lens                   ( makeLenses )
import           Control.Lens.Setter

import           Control.PUS.Classes

import qualified Data.ByteString.Char8         as BC
import           Data.Attoparsec.ByteString     ( Parser )
import qualified Data.Attoparsec.ByteString    as A
import qualified Data.Attoparsec.Binary        as A
import           ByteString.StrictBuilder
import           Data.Conduit
import           Data.Conduit.Attoparsec

import           Data.PUS.Types
import           Data.PUS.Events

import           Protocol.SizeOf

import           General.Padding
import General.Hexdump




data NcduHeaderType =
    NCDU_TC_PKT_TYPE
    | NCDU_PKT_RES_TYPE
    | NCDU_LNK_STAT_TYPE
    | NCDU_DIR_TYPE
    | NCDU_TE_TYPE
    | NCDU_TE_RES_TYPE
    | NCDU_TC_CLTU_TYPE
    | NCDU_CLTU_RES_TYPE
    | NCDU_TC_FSP_TYPE
    | NCDU_DIR_FSP_TYPE
    | NCDU_FSP_RES_TYPE
    | NCDU_FSP_DIR_RES_TYPE
    | NCDU_NIS_TE_TYPE
    | NCDU_NIS_TE_RES_TYPE
    | NCDU_TC_FRAME_TYPE
    | NCDU_FRAME_RES_TYPE
    | NCDU_ILLEGAL_TYPE
    deriving (Ord, Eq, Show, Read, Enum)



hdrLen :: Int
hdrLen = 4 + 2 + 2

cltuSecHdrLen :: Int
cltuSecHdrLen = 4 + 4 + 8 + 4 + 8 + 4 + 1 + 4 + 1 + 1

-- cltuHdrLen :: Word32
-- cltuHdrLen = hdrLen + cltuSecHdrLen

cltuRespLen :: Int
cltuRespLen = 8 + 3 + 4 + 3 + 4 + 4

-- cltuRespHdrLen :: Word32
-- cltuRespHdrLen = hdrLen + cltuRespLen

pktHdrLen :: Int
pktHdrLen = 7

tmHdrLen :: Int
tmHdrLen = 4 + 2 + 1 + 1 + 2 + 8 + 1 + 1

admMessageHdrLen :: Int
admMessageHdrLen = 4 + 8 + 2 + 2 + 4



data NcduTcHeader = NcduTcHeader {
    _ncduPktSize :: !Word32,
    _ncduType :: !NcduHeaderType,
    _ncduScID :: !SCID
    } deriving (Show, Read)
makeLenses ''NcduTcHeader

instance FixedSize NcduTcHeader where
    fixedSizeOf = hdrLen


data NcduTcCltuHeader = NcduTcCltuHeader {
    _ncduTcCltuTransType :: !TransmissionMode,
    _ncduTcCltuSeqCnt :: !Word32,
    _ncduTcCltuVcID :: !VCID,
    _ncduTcCltuMapID :: !MAPID,
    _ncduTcCltuAggrFlags :: !Word32,
    _ncduTcCltuEarliestProdTimeFlags :: !Word32,
    _ncduTcCltuEarliestProdTime :: !Word64,
    _ncduTcCltuLatestProdTimeFlag :: !Word32,
    _ncduTcCltuLatestProdTime :: !Word64,
    _ncduTcCltuDelay :: !Word32
    }
    deriving (Show, Read)
makeLenses ''NcduTcCltuHeader

instance FixedSize NcduTcCltuHeader where
    fixedSizeOf = cltuSecHdrLen


data NcduDirectiveType =
      NCDU_UNLOCK_DIRTYPE
    | NCDU_SETVR_DIRTYPE
    | NCDU_ADTER_DIRTYPE
    deriving (Ord, Eq, Show, Read)


data NcduTcPktHeader = NcduTcPktHeader {
    _ncduTcPktTransType :: !TransmissionMode,
    _ncduTcPktSeqCnt :: !Word32,
    _ncduTcPktVcID :: !VCID,
    _ncduTcPktMapID :: !MAPID
    }
    deriving (Show, Read)
makeLenses ''NcduTcPktHeader

instance FixedSize NcduTcPktHeader where
    fixedSizeOf = pktHdrLen



data NcduTmStreamType =
    NCDU_TM_SLC_TIMELY
    | NCDU_TM_MC_TIMELY
    | NCDU_TM_VC_TIMELY
    | NCDU_TM_BADFRAME_TIMELY
    | NCDU_TM_SLC_COMPLETE
    | NCDU_TM_MC_COMPLETE
    | NCDU_TM_VC_COMPLETE
    | NCDU_TM_BADFRAME_COMPLETE
    | NCDU_TM_SLC_OFFLINE
    | NCDU_TM_MC_OFFLINE
    | NCDU_TM_VC_OFFLINE
    | NCDU_TM_BADFRAME_OFFLINE
    | NCDU_TM_UNKNOWN_VALUE
    deriving (Ord, Eq, Enum, Show, Read)



data NcduTmDuHeader = NcduTmDuHeader {
    _ncduTmSize :: !Word32,
    _ncduTmScID :: !SCID,
    _ncduTmDataStreamType :: !NcduTmStreamType,
    _ncduTmVcID :: !VCID,
    _ncduTmGsID :: !Word16,
    _ncduTmERT :: !Word64,
    _ncduTmSeqFlag :: !Word8,
    _ncduTmQualityFlag :: !Word8
    } deriving (Show, Read)
makeLenses ''NcduTmDuHeader

instance FixedSize NcduTmDuHeader where
    fixedSizeOf = tmHdrLen


data NcduTmDu = NcduTmDu {
    _ncduTmHeader :: NcduTmDuHeader,
    _ncduTmData :: !ByteString
} deriving (Show, Read)
makeLenses ''NcduTmDu

instance SizeOf NcduTmDu where
    sizeof NcduTmDu {..} =
        B.length _ncduTmData + fixedSizeOf @NcduTmDuHeader


data NcduTcCltuRespAck =
    NCDU_CLTU_RESP_CONFIRM_ACCEPT
    | NCDU_CLTU_RESP_CONFIRM_TRANSMIT
    | NCDU_CLTU_RESP_CONFIRM_TRANSFER
    | NCDU_CLTU_RESP_FAILURE_ACCEPT
    | NCDU_CLTU_RESP_FAILURE_TRANSMIT
    | NCDU_CLTU_RESP_FAILURE_TRANSFER
    | NCDU_CLTU_RESP_REJECT
    deriving (Ord, Eq, Show, Read, Enum)


data NcduTcCltuResponse = NcduTcCltuResponse {
    _ncduCltuRespTime :: !Word64,
    _ncduCltuRespTransType :: !TransmissionMode,
    _ncduCltuRespGsID :: !Word16,
    _ncduCltuRespTCID :: !Word32,
    _ncduCltuRespAck :: !NcduTcCltuRespAck,
    _ncduCltuRespReason :: !Word8,
    _ncduCltuRespSpaceInQueue :: !Word8,
    _ncduCltuRespNextTCID :: !Word32,
    _ncduCltuLastCLCW :: !Word32
    } deriving (Show, Read)
makeLenses ''NcduTcCltuResponse

instance FixedSize NcduTcCltuResponse where
    fixedSizeOf = cltuRespLen


data NcduAdminMessageType = NCDU_ADM_TM | NCDU_ADM_TC deriving (Ord, Eq, Enum, Show, Read)


data NcduAdminMessageSeverity =
    NCDU_INFO
    | NCDU_WARNING
    | NCDU_ALARM
    deriving (Ord, Eq, Enum, Show, Read)


data NcduAdminMessage = NcduAdminMessage {
    _ncduAdmLength :: !Word32,
    _ncduAdmTime :: !Word64,
    _ncduAdmType :: !NcduAdminMessageType,
    _ncduAdmSeverity :: !NcduAdminMessageSeverity,
    _ncduAdmEventID :: !Word32,
    _ncduAdmMsg :: !BC.ByteString
    } deriving (Show, Read)
makeLenses ''NcduAdminMessage

instance SizeOf NcduAdminMessage where
    sizeof NcduAdminMessage {..} =
        B.length _ncduAdmMsg + admMessageHdrLen


data NcduTcData =
    NcduTcDuPktData {
        _ncduTcPktHdr :: !NcduTcPktHeader,
        _ncduTcPktData :: !ByteString}
    | NcduTcDuCltuData {
        _ncduTcCltuHdr :: !NcduTcCltuHeader,
        _ncduTcCltuData :: !ByteString}
    | NcduTcDuCltuRespData {
        _ncduTcCltuResp :: !NcduTcCltuResponse}
    deriving (Show, Read)
makeLenses ''NcduTcData

instance SizeOf NcduTcData where
    sizeof NcduTcDuPktData {..} =
        B.length _ncduTcPktData + fixedSizeOf @NcduTcPktHeader
    sizeof NcduTcDuCltuData {..} =
        B.length _ncduTcCltuData + fixedSizeOf @NcduTcCltuHeader
    sizeof NcduTcDuCltuRespData {..} = fixedSizeOf @NcduTcCltuResponse


data NcduTcDu = NcduTcDu {
    _ncduTcHdr :: !NcduTcHeader,
    _ncduTcData :: NcduTcData
    } deriving (Show, Read)
makeLenses ''NcduTcDu

instance SizeOf NcduTcDu where
    sizeof NcduTcDu {..} =
        fixedSizeOf @NcduTcHeader + sizeof _ncduTcData


receiveTcNcduC
    :: (MonadIO m, MonadReader env m, HasGlobalState env)
    => ConduitT ByteString NcduTcDu m ()
receiveTcNcduC = conduitParserEither ncduTcParser .| sink
  where
    sink = do
        x <- await
        case x of
            Just tc -> case tc of
                Left err -> do
                    st <- ask
                    liftIO $ raiseEvent st $ EVAlarms (EVNCDUParseError (T.pack (errorMessage err)))
                    sink
                Right (_, tc') -> do
                    yield tc'
                    sink
            Nothing -> pure ()

receiveTmNcduC
    :: (MonadIO m, MonadReader env m, HasGlobalState env)
    => ConduitT ByteString NcduTmDu m ()
receiveTmNcduC = conduitParserEither ncduTmParser .| sink
  where
    sink = do
        x <- await
        case x of
            Just tc -> case tc of
                Left err -> do
                    st <- ask
                    liftIO $ raiseEvent st $ EVAlarms (EVNCDUParseError (T.pack (errorMessage err)))
                    sink
                Right (_, tc') -> do
                    yield tc'
                    sink
            Nothing -> pure ()

receiveAdminNcduC
    :: (MonadIO m, MonadReader env m, HasGlobalState env)
    => ConduitT ByteString NcduAdminMessage m ()
receiveAdminNcduC = conduitParserEither ncduAdminMessageParser .| sink
  where
    sink = do
        x <- await
        case x of
            Just tc -> case tc of
                Left err -> do
                    st <- ask
                    liftIO $ raiseEvent st $ EVAlarms (EVNCDUParseError (T.pack (errorMessage err)))
                    sink
                Right (_, tc') -> do
                    yield tc'
                    sink
            Nothing -> pure ()


encodeTcNcduC :: (MonadIO m, MonadReader env m, HasLogFunc env) => ConduitT NcduTcDu ByteString m ()
encodeTcNcduC = awaitForever $ \du -> do
    let enc = builderBytes (ncduTcDuBuilder du)
    logDebug $ "Encoded NCDU: " <> displayShow du <> ":\n" <> display (hexdumpBS enc)
    yield enc

encodeTmNcduC :: (Monad m) => ConduitT NcduTmDu ByteString m ()
encodeTmNcduC = awaitForever $ \du -> do
    let enc = builderBytes (ncduTmDuBuilder du)
    yield enc

encodeAdminNcduC :: (Monad m) => ConduitT NcduAdminMessage ByteString m ()
encodeAdminNcduC = awaitForever $ \du -> do
    let enc = builderBytes (ncduAdminMessageBuilder du)
    yield enc


ncduTcHeaderParser :: Parser NcduTcHeader
ncduTcHeaderParser = do
    len  <- A.anyWord32be
    typ  <- convertToType <$> A.anyWord16be
    scid <- scidParser
    return $! NcduTcHeader len typ scid

ncduTcCltuHeaderParser :: Parser NcduTcCltuHeader
ncduTcCltuHeaderParser = do
    tt    <- A.anyWord8
    seqc  <- A.anyWord32be
    vcid  <- mkVCID <$> A.anyWord8
    mapid <- mkMAPID <$> A.anyWord8
    a     <- A.anyWord32be
    epf   <- A.anyWord32be
    eft   <- A.anyWord64be
    lpf   <- A.anyWord32be
    lpt   <- A.anyWord64be
    NcduTcCltuHeader (convertToTransType tt) seqc vcid mapid a epf eft lpf lpt
        <$> A.anyWord32be


ncduTcCltuResponseParser :: Parser NcduTcCltuResponse
ncduTcCltuResponseParser = do
    respTime  <- A.anyWord64be
    transType <- transmissionModeParser
    gsid      <- A.anyWord16be
    tcid      <- A.anyWord32be
    ack       <- ncduRespAckParser
    reason    <- A.anyWord8
    spiq      <- A.anyWord8
    nextTcID  <- A.anyWord32be
    NcduTcCltuResponse respTime transType gsid tcid ack reason spiq nextTcID
        <$> A.anyWord32be


ncduRespAckParser :: Parser NcduTcCltuRespAck
ncduRespAckParser = convertToCltuRespAck <$> A.anyWord8

ncduTcPktHeaderParser :: Parser NcduTcPktHeader
ncduTcPktHeaderParser = do
    transType <- transmissionModeParser
    seqCnt    <- A.anyWord32be
    vcID      <- mkVCID <$> A.anyWord8
    NcduTcPktHeader transType seqCnt vcID . mkMAPID <$> A.anyWord8


ncduTcParser :: Parser NcduTcDu
ncduTcParser = do
    hdr <- ncduTcHeaderParser

    let len = msgCalcPayloadLen hdr

    dat <- case _ncduType hdr of
        NCDU_TC_CLTU_TYPE -> do
            chdr <- ncduTcCltuHeaderParser
            payl <- A.take (len - cltuSecHdrLen)
            pure $ NcduTcDuCltuData chdr payl
        NCDU_CLTU_RES_TYPE -> NcduTcDuCltuRespData <$> ncduTcCltuResponseParser
        NCDU_TC_FRAME_TYPE -> do
            chdr <- ncduTcCltuHeaderParser
            payl <- A.take (len - cltuSecHdrLen)
            pure $ NcduTcDuCltuData chdr payl
        _ -> do
            phdr <- ncduTcPktHeaderParser
            payl <- A.take (len - pktHdrLen)
            pure $ NcduTcDuPktData phdr payl
    pure (NcduTcDu hdr dat)

ncduTmHeaderParser :: Parser NcduTmDuHeader
ncduTmHeaderParser = do
    size       <- A.anyWord32be
    scid       <- scidParser
    streamType <- ncduStreamTypeParser
    vcID       <- vcidParser
    gsID       <- A.anyWord16be
    ert        <- A.anyWord64be
    seqFlags   <- A.anyWord8
    NcduTmDuHeader size scid streamType vcID gsID ert seqFlags <$> A.anyWord8

ncduStreamTypeParser :: Parser NcduTmStreamType
ncduStreamTypeParser = toEnum . fromIntegral <$> A.anyWord8

ncduTmParser :: Parser NcduTmDu
ncduTmParser = do
    hdr <- ncduTmHeaderParser
    let len = fromIntegral (_ncduTmSize hdr) - fixedSizeOf @NcduTmDuHeader
    dat <- A.take len
    return $! NcduTmDu hdr dat


convertType :: NcduHeaderType -> Word16
convertType NCDU_TC_PKT_TYPE      = 0
convertType NCDU_PKT_RES_TYPE     = 1
convertType NCDU_LNK_STAT_TYPE    = 3
convertType NCDU_DIR_TYPE         = 4
convertType NCDU_TE_TYPE          = 5
convertType NCDU_TE_RES_TYPE      = 6
convertType NCDU_TC_CLTU_TYPE     = 7
convertType NCDU_CLTU_RES_TYPE    = 8
convertType NCDU_TC_FSP_TYPE      = 20
convertType NCDU_DIR_FSP_TYPE     = 22
convertType NCDU_FSP_RES_TYPE     = 21
convertType NCDU_FSP_DIR_RES_TYPE = 23
convertType NCDU_NIS_TE_TYPE      = 30
convertType NCDU_NIS_TE_RES_TYPE  = 31
convertType NCDU_TC_FRAME_TYPE    = 50
convertType NCDU_FRAME_RES_TYPE   = 51
convertType NCDU_ILLEGAL_TYPE     = 0xFFFF


convertToType :: Word16 -> NcduHeaderType
convertToType x = case x of
    0  -> NCDU_TC_PKT_TYPE
    1  -> NCDU_PKT_RES_TYPE
    3  -> NCDU_LNK_STAT_TYPE
    4  -> NCDU_DIR_TYPE
    5  -> NCDU_TE_TYPE
    6  -> NCDU_TE_RES_TYPE
    7  -> NCDU_TC_CLTU_TYPE
    8  -> NCDU_CLTU_RES_TYPE
    20 -> NCDU_TC_FSP_TYPE
    22 -> NCDU_DIR_FSP_TYPE
    21 -> NCDU_FSP_RES_TYPE
    23 -> NCDU_FSP_DIR_RES_TYPE
    30 -> NCDU_NIS_TE_TYPE
    31 -> NCDU_NIS_TE_RES_TYPE
    50 -> NCDU_TC_FRAME_TYPE
    51 -> NCDU_FRAME_RES_TYPE
    _  -> NCDU_ILLEGAL_TYPE


convertTransType :: TransmissionMode -> Word8
convertTransType AD = 0
convertTransType BD = 1


convertToTransType :: Word8 -> TransmissionMode
convertToTransType x = case x of
    0 -> AD
    1 -> BD
    _ -> BD


convertCltuRespAck :: NcduTcCltuRespAck -> Word8
convertCltuRespAck NCDU_CLTU_RESP_CONFIRM_ACCEPT   = 0
convertCltuRespAck NCDU_CLTU_RESP_CONFIRM_TRANSMIT = 1
convertCltuRespAck NCDU_CLTU_RESP_CONFIRM_TRANSFER = 2
convertCltuRespAck NCDU_CLTU_RESP_FAILURE_ACCEPT   = 3
convertCltuRespAck NCDU_CLTU_RESP_FAILURE_TRANSMIT = 4
convertCltuRespAck NCDU_CLTU_RESP_FAILURE_TRANSFER = 5
convertCltuRespAck NCDU_CLTU_RESP_REJECT           = 9

convertToCltuRespAck :: Word8 -> NcduTcCltuRespAck
convertToCltuRespAck x = case x of
    0 -> NCDU_CLTU_RESP_CONFIRM_ACCEPT
    1 -> NCDU_CLTU_RESP_CONFIRM_TRANSMIT
    2 -> NCDU_CLTU_RESP_CONFIRM_TRANSFER
    3 -> NCDU_CLTU_RESP_FAILURE_ACCEPT
    4 -> NCDU_CLTU_RESP_FAILURE_TRANSMIT
    5 -> NCDU_CLTU_RESP_FAILURE_TRANSFER
    9 -> NCDU_CLTU_RESP_REJECT
    _ -> NCDU_CLTU_RESP_REJECT

msgCalcPayloadLen :: NcduTcHeader -> Int
msgCalcPayloadLen hdr = fromIntegral (_ncduPktSize hdr) - hdrLen



ncduTcHeaderBuilder :: NcduTcHeader -> Builder
ncduTcHeaderBuilder x =
    word32BE (_ncduPktSize x)
        <> word16BE (convertType . _ncduType $ x)
        <> scidBuilder (_ncduScID x)


ncduTcCltuHeaderBuilder :: NcduTcCltuHeader -> Builder
ncduTcCltuHeaderBuilder x =
    word8 (convertTransType . _ncduTcCltuTransType $ x)
        <> word32BE (_ncduTcCltuSeqCnt x)
        <> vcidBuilder (_ncduTcCltuVcID x)
        <> mapIDBuilder (_ncduTcCltuMapID x)
        <> word32BE (_ncduTcCltuAggrFlags x)
        <> word32BE (_ncduTcCltuEarliestProdTimeFlags x)
        <> word64BE (_ncduTcCltuEarliestProdTime x)
        <> word32BE (_ncduTcCltuLatestProdTimeFlag x)
        <> word64BE (_ncduTcCltuLatestProdTime x)
        <> word32BE (_ncduTcCltuDelay x)

ncduTcPktHeaderBuilder :: NcduTcPktHeader -> Builder
ncduTcPktHeaderBuilder x =
    transmissionModeBuilder (_ncduTcPktTransType x)
        <> word32BE (_ncduTcPktSeqCnt x)
        <> vcidBuilder (_ncduTcPktVcID x)
        <> mapIDBuilder (_ncduTcPktMapID x)


ncduTcCltuResponseBuilder :: NcduTcCltuResponse -> Builder
ncduTcCltuResponseBuilder x =
    word64BE (_ncduCltuRespTime x)
        <> transmissionModeBuilder (_ncduCltuRespTransType x)
        <> word16BE (_ncduCltuRespGsID x)
        <> word32BE (_ncduCltuRespTCID x)
        <> word8 (convertCltuRespAck . _ncduCltuRespAck $ x)
        <> word8 (_ncduCltuRespReason x)
        <> word8 (_ncduCltuRespSpaceInQueue x)
        <> word32BE (_ncduCltuRespNextTCID x)
        <> word32BE (_ncduCltuLastCLCW x)


ncduTcDuBuilder :: NcduTcDu -> Builder
ncduTcDuBuilder x@(NcduTcDu hdr dat) =
    let size   = fromIntegral (sizeof x)
        newHdr = hdr & ncduPktSize .~ size
    in  ncduTcHeaderBuilder newHdr <> ncduTcDataBuilder dat

ncduTcDataBuilder :: NcduTcData -> Builder
ncduTcDataBuilder (NcduTcDuPktData phdr payl) =
    ncduTcPktHeaderBuilder phdr <> bytes payl
ncduTcDataBuilder (NcduTcDuCltuData chdr payl) =
    ncduTcCltuHeaderBuilder chdr <> bytes payl
ncduTcDataBuilder (NcduTcDuCltuRespData cresp) =
    ncduTcCltuResponseBuilder cresp


ncduTmDuHeaderBuilder :: NcduTmDuHeader -> Builder
ncduTmDuHeaderBuilder x =
    word32BE (_ncduTmSize x)
        <> scidBuilder (_ncduTmScID x)
        <> ncduTmStreamTypeBuilder (_ncduTmDataStreamType x)
        <> vcidBuilder (_ncduTmVcID x)
        <> word16BE (_ncduTmGsID x)
        <> word64BE (_ncduTmERT x)
        <> word8 (_ncduTmSeqFlag x)
        <> word8 (_ncduTmQualityFlag x)

ncduTmDuBuilder :: NcduTmDu -> Builder
ncduTmDuBuilder x =
    let size   = fromIntegral (sizeof x)
        newHdr = _ncduTmHeader x & ncduTmSize .~ size
    in  ncduTmDuHeaderBuilder newHdr <> bytes (_ncduTmData x)


convertTmStreamType :: NcduTmStreamType -> Word8
convertTmStreamType x = case x of
    NCDU_TM_SLC_TIMELY        -> 0
    NCDU_TM_MC_TIMELY         -> 1
    NCDU_TM_VC_TIMELY         -> 2
    NCDU_TM_BADFRAME_TIMELY   -> 3
    NCDU_TM_SLC_COMPLETE      -> 4
    NCDU_TM_MC_COMPLETE       -> 5
    NCDU_TM_VC_COMPLETE       -> 6
    NCDU_TM_BADFRAME_COMPLETE -> 7
    NCDU_TM_SLC_OFFLINE       -> 8
    NCDU_TM_MC_OFFLINE        -> 9
    NCDU_TM_VC_OFFLINE        -> 10
    NCDU_TM_BADFRAME_OFFLINE  -> 11
    NCDU_TM_UNKNOWN_VALUE     -> 255

-- convertToTmStreamType :: Word8 -> NcduTmStreamType
-- convertToTmStreamType x = case x of
--     0  -> NCDU_TM_SLC_TIMELY
--     1  -> NCDU_TM_MC_TIMELY
--     2  -> NCDU_TM_VC_TIMELY
--     3  -> NCDU_TM_BADFRAME_TIMELY
--     4  -> NCDU_TM_SLC_COMPLETE
--     5  -> NCDU_TM_MC_COMPLETE
--     6  -> NCDU_TM_VC_COMPLETE
--     7  -> NCDU_TM_BADFRAME_COMPLETE
--     8  -> NCDU_TM_SLC_OFFLINE
--     9  -> NCDU_TM_MC_OFFLINE
--     10 -> NCDU_TM_VC_OFFLINE
--     11 -> NCDU_TM_BADFRAME_OFFLINE
--     _  -> NCDU_TM_UNKNOWN_VALUE


-- ncduDirectiveTypeBuilder :: NcduDirectiveType -> Builder
-- ncduDirectiveTypeBuilder NCDU_UNLOCK_DIRTYPE = word8 2
-- ncduDirectiveTypeBuilder NCDU_SETVR_DIRTYPE  = word8 3
-- ncduDirectiveTypeBuilder NCDU_ADTER_DIRTYPE  = word8 4


-- ncduTmStreamTypeParser :: Parser NcduTmStreamType
-- ncduTmStreamTypeParser = convertToTmStreamType <$> A.anyWord8

ncduTmStreamTypeBuilder :: NcduTmStreamType -> Builder
ncduTmStreamTypeBuilder x = word8 (convertTmStreamType x)


ncduAdminMessageTypeBuilder :: NcduAdminMessageType -> Builder
ncduAdminMessageTypeBuilder NCDU_ADM_TM = word16BE 0
ncduAdminMessageTypeBuilder NCDU_ADM_TC = word16BE 1

ncduAdminMessageTypeParser :: Parser NcduAdminMessageType
ncduAdminMessageTypeParser = do
    val <- A.anyWord16be
    let v = case val of
            0 -> NCDU_ADM_TM
            1 -> NCDU_ADM_TC
            _ -> NCDU_ADM_TM
    return v


ncduAdminMessageSeverityBuilder :: NcduAdminMessageSeverity -> Builder
ncduAdminMessageSeverityBuilder NCDU_INFO    = word16BE 0
ncduAdminMessageSeverityBuilder NCDU_WARNING = word16BE 1
ncduAdminMessageSeverityBuilder NCDU_ALARM   = word16BE 2


ncduAdminMessageSeverityParser :: Parser NcduAdminMessageSeverity
ncduAdminMessageSeverityParser = do
    val <- A.anyWord16be
    let v = case val of
            0 -> NCDU_INFO
            1 -> NCDU_WARNING
            2 -> NCDU_ALARM
            _ -> NCDU_INFO
    return v


ncduAdminMessageBuilder :: NcduAdminMessage -> Builder
ncduAdminMessageBuilder x =
    let size   = fromIntegral (sizeof x)
        newLen = 1 + size
    in  word32BE newLen
            <> word64BE (_ncduAdmTime x)
            <> ncduAdminMessageTypeBuilder (_ncduAdmType x)
            <> ncduAdminMessageSeverityBuilder (_ncduAdmSeverity x)
            <> word32BE (_ncduAdmEventID x)
            <> bytes (_ncduAdmMsg x)
            <> word8 0

ncduAdminMessageParser :: Parser NcduAdminMessage
ncduAdminMessageParser = do
    l    <- A.anyWord32be
    t    <- A.anyWord64be
    tp   <- ncduAdminMessageTypeParser
    sv   <- ncduAdminMessageSeverityParser
    ev   <- A.anyWord32be
    payl <- A.take (fromIntegral l - admMessageHdrLen)
    return $ NcduAdminMessage l t tp sv ev payl


ncduAdminMsgTcEstablished :: ByteString -> Word32 -> NcduAdminMessage
ncduAdminMsgTcEstablished gsName adCnt = NcduAdminMessage
    0
    0
    NCDU_ADM_TC
    NCDU_INFO
    1
    (builderBytes establishedMessage)
  where
    establishedMessage =
        bytes "Established TC link to "
            <> bytes (leftPaddedC ' ' 8 gsName)
            <> bytes ":seq.count= "
            <> bytes (leftPaddedC ' ' 10 ((BC.pack . show) adCnt))

ncduAdminMsgTcClosed :: ByteString -> NcduAdminMessage
ncduAdminMsgTcClosed gsName = NcduAdminMessage 0
                                               0
                                               NCDU_ADM_TC
                                               NCDU_INFO
                                               3
                                               (builderBytes message)
  where
    message = bytes "Closed TC link to " <> bytes (leftPaddedC ' ' 8 gsName)

ncduAdminMsgTcAborted :: ByteString -> NcduAdminMessage
ncduAdminMsgTcAborted gsName = NcduAdminMessage 0
                                                0
                                                NCDU_ADM_TC
                                                NCDU_INFO
                                                4
                                                (builderBytes message)
  where
    message = bytes "Aborted TC link to " <> bytes (leftPaddedC ' ' 8 gsName)


ncduAdminMsgTcAbortedFromGS :: ByteString -> NcduAdminMessage
ncduAdminMsgTcAbortedFromGS gsName = NcduAdminMessage
    0
    0
    NCDU_ADM_TC
    NCDU_WARNING
    5
    (builderBytes message)
  where
    message = bytes (leftPaddedC ' ' 8 gsName) <> bytes "aborted TC link (xxx)"

ncduAdminMsgTcADavailable :: Word8 -> [Word8] -> ByteString -> NcduAdminMessage
ncduAdminMsgTcADavailable vcid mapidLst gsName = NcduAdminMessage
    0
    0
    NCDU_ADM_TC
    NCDU_INFO
    6
    (builderBytes message)
  where
    message =
        bytes "AD service "
            <> bytes (leftPaddedC ' ' 2 (BC.pack (show vcid)))
            <> bytes " available from "
            <> bytes (leftPaddedC ' ' 8 gsName)
            <> bytes ": MAPids="
            <> mapids
    mapids = mconcat $ intersperse (bytes ",") $ map
        (bytes . leftPaddedC ' ' 2 . BC.pack . show)
        mapidLst

ncduAdminMsgTmTMFLOW :: NcduAdminMessage
ncduAdminMsgTmTMFLOW =
    NcduAdminMessage 0 0 NCDU_ADM_TM NCDU_INFO 1 "Set TM link status to TM FLOW"

ncduAdminMsgTmNOTMFLOW :: NcduAdminMessage
ncduAdminMsgTmNOTMFLOW = NcduAdminMessage
    0
    0
    NCDU_ADM_TM
    NCDU_WARNING
    2
    "Set TM link status to NO TM FLOW"
