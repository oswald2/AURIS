{-# LANGUAGE
    OverloadedStrings
    , DeriveGeneric
    , GeneralizedNewtypeDeriving
    , NoImplicitPrelude
    , TemplateHaskell
    , BangPatterns
    , MultiWayIf
    , TypeApplications
#-}
module Data.PUS.PUSPacket
    ( PUSPacket(..)
    , PUSHeader(..)
    , PUSPacketType(..)
    , TMPIVal(..)
    , HasCRC(..)
    , encodePUSPacket
    , encodePUSPktChoice
    , encodePktWithoutCRC
    , decodePktMissionSpecific
    , pusHdr
    , pusDfh
    , pusPIs
    , pusData
    , pusHdrPktID
    , pusHdrTcVersion
    , pusHdrType
    , pusHdrDfhFlag
    , pusHdrAPID
    , pusHdrSeqFlags
    , pusHdrSSC
    , pusHdrSeqCtrl
    , pusHdrTcLength
    , pusPktHdrBuilder
    , pusPktHdrParser
    , pusPktHdrParserWithoutPktID
    , pusPktHdrLenOnlyParser
    , pusPktParser
    , pusPktParserPayload
    , cncPusPktParserPayload
    , pusPktIdleAPID
    , pusPktTimePktAPID
    , pusPktIsIdle
    , pusEncodeCRC
    , headPacket
    , chunkPackets
    , packPktID
    , packSeqFlags
    ) where


import           Data.Vector.Storable.ByteString
import           RIO                     hiding ( (.~)
                                                , Builder
                                                )
import qualified RIO.ByteString                as B
import qualified RIO.Text                      as T
import qualified RIO.Vector.Storable           as V
import qualified RIO.Vector.Storable.Unsafe    as V
                                                ( unsafeFreeze )

import           ByteString.StrictBuilder       ( Builder
                                                , builderBytes
                                                , bytes
                                                , word16BE
                                                )
import           Codec.Serialise
import           Control.Lens                   ( (.~)
                                                , makeLenses
                                                )
import           Data.Aeson
import qualified Data.Attoparsec.Binary        as A
import           Data.Attoparsec.ByteString     ( Parser )
import qualified Data.Attoparsec.ByteString    as A
import           Data.Bits                      ( Bits
                                                    ( (.&.)
                                                    , (.|.)
                                                    , shiftL
                                                    , shiftR
                                                    )
                                                )
import qualified Text.Builder                  as T

import           Data.PUS.CRC                   ( CRC
                                                , crcCheck
                                                , crcEncodeAndAppendBS
                                                , crcLen
                                                )
import           Data.PUS.EncTime               ( CucEncoding(..) )
import           Data.PUS.MissionSpecific.Definitions
                                                ( PUSMissionSpecific
                                                , pmsTCDataFieldHeader
                                                , pmsTMDataFieldHeader
                                                )
import           Data.PUS.PUSDfh
import           Data.PUS.SegmentationFlags     ( SegmentationFlags(..) )

import           General.APID                   ( APID(APID) )
import           General.PUSTypes               ( PUSPacketType(..)
                                                , PktID(..)
                                                , SSC
                                                , SeqControl(..)
                                                , getSSC
                                                , mkSSC
                                                )
import           General.SetBitField            ( setBitFieldR )
import           General.SizeOf                 ( FixedSize(..)
                                                , SizeOf(sizeof)
                                                )
import           General.Types                  ( BitOffsets(toBitOffset)
                                                , HasCRC(..)
                                                , HexBytes(..)
                                                , hasCRC
                                                , hexLength
                                                , unBitSize
                                                )

import           Protocol.ProtocolInterfaces    ( ProtocolInterface(IfCnc)
                                                , ProtocolPacket(ProtocolPacket)
                                                , isCnc
                                                , isEden
                                                , isNctrs
                                                , isNdiu
                                                , isSLE
                                                )

import           Data.TM.PIVals                 ( TMPIVal(..) )
--import           General.Hexdump




data PUSHeader = PUSHeader
    { _pusHdrPktID     :: !PktID
    , _pusHdrTcVersion :: !Word8
    , _pusHdrType      :: !PUSPacketType
    , _pusHdrDfhFlag   :: !Bool
    , _pusHdrAPID      :: !APID
    , _pusHdrSeqFlags  :: !SegmentationFlags
    , _pusHdrSSC       :: !SSC
    , _pusHdrSeqCtrl   :: !SeqControl
    , _pusHdrTcLength  :: !Word16
    }
    deriving (Show, Read, Generic)

makeLenses ''PUSHeader

-- we don't compare the fields pktID and SeqCtrl as these are only
-- convenience fields, which are only used when a packet is decoded.
-- they are set to 0 when a packet is created and therefore are
-- not necessary to compare
-- Also the length is not set when creating a packet, but will be
-- calculated on encoding.
instance Eq PUSHeader where
    p1 == p2 =
        _pusHdrTcVersion p1
            == _pusHdrTcVersion p2
            && _pusHdrType p1
            == _pusHdrType p2
            && _pusHdrDfhFlag p1
            == _pusHdrDfhFlag p2
            && _pusHdrAPID p1
            == _pusHdrAPID p2
            && _pusHdrSeqFlags p1
            == _pusHdrSeqFlags p2
            && _pusHdrSSC p1
            == _pusHdrSSC p2


instance Serialise PUSHeader
instance NFData PUSHeader
instance FromJSON PUSHeader
instance ToJSON PUSHeader where
    toEncoding = genericToEncoding defaultOptions

instance FixedSize PUSHeader where
    fixedSizeOf = 6


data PUSPacket = PUSPacket
    { _pusHdr       :: !PUSHeader
    , _pusDfh       :: !DataFieldHeader
    , _pusPIs       :: Maybe (TMPIVal, TMPIVal)
    , _pusData      :: !HexBytes
    , _pusEncodeCRC :: !Bool
    }
    deriving (Eq, Show, Read, Generic)
makeLenses ''PUSPacket

instance Serialise PUSPacket
instance NFData PUSPacket

instance FromJSON PUSPacket where
    parseJSON = withObject "PUSPacket" $ \v ->
        PUSPacket
            <$> v
            .:  "pusHdr"
            <*> v
            .:  "pusDfh"
            <*> v
            .:  "pusPIs"
            <*> v
            .:  "pusData"
            <*> v
            .:  "pusEncodeCRC"


instance ToJSON PUSPacket where
    toJSON r = object
        [ "pusHdr" .= _pusHdr r
        , "pusDfh" .= _pusDfh r
        , "pusPIs" .= _pusPIs r
        , "pusData" .= _pusData r
        , "pusEncodeCRC" .= _pusEncodeCRC r
        ]
    toEncoding r = pairs
        (  "pusHdr"
        .= _pusHdr r
        <> "pusDfh"
        .= _pusDfh r
        <> "pusPIs"
        .= _pusPIs r
        <> "pusData"
        .= _pusData r
        <> "pusEncodeCRC"
        .= _pusEncodeCRC r
        )



instance SizeOf PUSPacket where
    sizeof x =
        fixedSizeOf @PUSHeader + sizeof (_pusDfh x) + hexLength (_pusData x)


pusPktIdleAPID :: APID
pusPktIdleAPID = APID 0x7FF

pusPktTimePktAPID :: APID
pusPktTimePktAPID = APID 0

pusPktIsIdle :: PUSPacket -> Bool
pusPktIsIdle pkt = pkt ^. pusHdr . pusHdrAPID == pusPktIdleAPID

-- | encodes a packet and sets the PI1/2 values for correct identification.
-- Returns the encoded packet as well as the PacketID and the SSC field for
-- TC verification purpuses
{-# INLINABLE encodePUSPacket #-}
encodePUSPacket :: PUSPacket -> (ByteString, PktID, SeqControl)
encodePUSPacket pkt = if _pusEncodeCRC pkt
    then
        let (encPkt, pktId, ssc) = encodePktWithoutCRC pkt True
        in  (crcEncodeAndAppendBS encPkt, pktId, ssc)
    else encodePktWithoutCRC pkt False

-- | encodes a packet and sets the PI1/2 values for correct identification
{-# INLINABLE encodePUSPktChoice #-}
encodePUSPktChoice :: Bool -> PUSPacket -> (ByteString, PktID, SeqControl)
encodePUSPktChoice True pkt =
    let (!encPkt, pktId, ssc) = encodePktWithoutCRC pkt True
    in  (crcEncodeAndAppendBS encPkt, pktId, ssc)
encodePUSPktChoice False pkt = encodePktWithoutCRC pkt False



-- | encodes' a packet, but does not calculate and append the CRC. The flag
-- | @useCRC is given for the case there is further processing of the packet
-- | before adding the CRC. Actually, if the flag is True, the length will be
-- | calculated differently, as the CRC is omitted.
-- | So if you need to append the CRC later, this flag should be True, otherwise
-- | False
{-# INLINABLE encodePktWithoutCRC #-}
encodePktWithoutCRC :: PUSPacket -> Bool -> (ByteString, PktID, SeqControl)
encodePktWithoutCRC pkt useCRC =
    let newPkt               = pusPktUpdateLen pkt useCRC
        (encHdr, pktId, ssc) = pusPktHdrBuilder (newPkt ^. pusHdr)
        encDfh               = dfhBuilder (newPkt ^. pusDfh)
        payload              = toBS (newPkt ^. pusData)
        !pl = builderBytes $ if newPkt ^. pusHdr . pusHdrDfhFlag
            then encHdr <> encDfh <> bytes payload
            else encHdr <> bytes payload
    in  (applied pkt pl, pktId, ssc)
  where
    applied PUSPacket { _pusPIs = Nothing } encPkt = encPkt
    applied PUSPacket { _pusPIs = Just pis@(pi1, pi2) } encPkt =
        if _tmpiOffset pi1 < 0 && _tmpiOffset pi2 < 0
            then encPkt
            else applyPIvals encPkt pis



-- | Takes a packet and a useCRC bool and returns the length. The length is
-- | according to the PUS standard the length of the application field
-- | (DFH length + payload + CRC if the bool is set)
{-# INLINABLE pusPktCalcLen #-}
pusPktCalcLen :: PUSPacket -> Bool -> Word16
pusPktCalcLen pkt useCRC =
    let
        newLen =
            dfhLen + hexLength (_pusData pkt) + if useCRC then crcLen else 0
        dfhLen =
            if pkt ^. pusHdr . pusHdrDfhFlag then dfhLength (_pusDfh pkt) else 0
    in
        fromIntegral (newLen - 1)


-- | Takes a packet and useCRC, calculates the length and updates the header
-- | with the new length
{-# INLINABLE pusPktUpdateLen #-}
pusPktUpdateLen :: PUSPacket -> Bool -> PUSPacket
pusPktUpdateLen pkt useCRC =
    let newLen = pusPktCalcLen pkt useCRC
    in  pkt & pusHdr . pusHdrTcLength .~ newLen



-- | sets the PI1 and PI2 values in a already encoded packet. This has
-- | to be done before CRC calculation
{-# INLINABLE applyPIvals #-}
applyPIvals :: ByteString -> (TMPIVal, TMPIVal) -> ByteString
applyPIvals encPkt pic = worker
  where
    worker =
        let v1 :: V.Vector Word8
            v1 = byteStringToVector encPkt
            v2 = runST $ do
                v <- V.thaw v1
                proc v pic >>= V.unsafeFreeze
        in  vectorToByteString v2


    proc v (TMPIVal val1 off1 wid1, TMPIVal val2 off2 wid2) = do
        v1 <- if off1 >= 0
            then setBitFieldR v
                              (toBitOffset off1)
                              (fromIntegral (unBitSize wid1))
                              (fromIntegral val1)
            else return v

        if off2 >= 0
            then setBitFieldR v1
                              (toBitOffset off2)
                              (fromIntegral (unBitSize wid2))
                              (fromIntegral val2)
            else return v1



{-# INLINABLE pusPktHdrBuilder #-}
pusPktHdrBuilder :: PUSHeader -> (Builder, PktID, SeqControl)
pusPktHdrBuilder hdr =
    let !pktId = packPktID (_pusHdrTcVersion hdr)
                           (_pusHdrType hdr)
                           (_pusHdrDfhFlag hdr)
                           (_pusHdrAPID hdr)
        !seqFlags = packSeqFlags (_pusHdrSeqFlags hdr) (_pusHdrSSC hdr)
    in  ( word16BE pktId <> word16BE seqFlags <> word16BE (_pusHdrTcLength hdr)
        , PktID pktId
        , SeqControl seqFlags
        )



{-# INLINABLE pusPktHdrParser #-}
pusPktHdrParser :: Parser PUSHeader
pusPktHdrParser = do
    pktId <- A.anyWord16be
    pusPktHdrParserWithoutPktID pktId


{-# INLINABLE pusPktHdrParserWithoutPktID #-}
pusPktHdrParserWithoutPktID :: Word16 -> Parser PUSHeader
pusPktHdrParserWithoutPktID pktId = do
    seqFlags <- A.anyWord16be
    len      <- A.anyWord16be

    let (!vers, !tp, !dfh, !apid) = unpackPktID pktId
        (!sf, !ssc)               = unpackSeqFlags seqFlags

    return
        (PUSHeader (PktID pktId)
                   vers
                   tp
                   dfh
                   apid
                   sf
                   ssc
                   (SeqControl seqFlags)
                   len
        )


{-# INLINABLE pusPktHdrLenOnlyParser #-}
pusPktHdrLenOnlyParser :: Parser Word16
pusPktHdrLenOnlyParser = do
    void $ A.take 4
    A.anyWord16be



{-# INLINABLE headPacket #-}
headPacket :: ByteString -> (ByteString, ByteString)
headPacket bs = case A.parseOnly pusPktHdrLenOnlyParser bs of
    Left _ -> (B.empty, bs)
    Right len ->
        let splitPoint = fromIntegral len + 1 + 6
        in  if splitPoint > B.length bs
                then (B.empty, bs)
                else B.splitAt splitPoint bs

{-# INLINABLE chunkPackets #-}
chunkPackets :: ByteString -> ([ByteString], ByteString)
chunkPackets bs = go bs []
  where
    go bs' acc =
        let (pkt, rest) = headPacket bs'
        in  if B.null pkt then (reverse acc, rest) else go rest (pkt : acc)


{-# INLINABLE packPktID #-}
packPktID :: Word8 -> PUSPacketType -> Bool -> APID -> Word16
packPktID !vers !tp !dfh (APID apid) =
    let versShifted = fromIntegral vers `shiftL` 13
        typeShifted = case tp of
            PUSTM -> 0x0000
            PUSTC -> 0x1000
        dfhShifted = if dfh then 0x0800 else 0x0000
        apidMasked = apid .&. 0x7ff
    in  versShifted .|. typeShifted .|. dfhShifted .|. apidMasked



{-# INLINABLE unpackPktID #-}
unpackPktID :: Word16 -> (Word8, PUSPacketType, Bool, APID)
unpackPktID pktID =
    let !apid = pktID .&. 0x7ff
        !dfh  = (pktID .&. 0x0800) /= 0
        !tp   = if (pktID .&. 0x1000) /= 0 then PUSTC else PUSTM
        !ver  = (pktID .&. 0xe000) `shiftR` 13
    in  (fromIntegral ver, tp, dfh, APID apid)



{-# INLINABLE packSeqFlags #-}
packSeqFlags :: SegmentationFlags -> SSC -> Word16
packSeqFlags !flags !ssc = bfl flags .|. getSSC ssc .&. 0x3FFF
  where
    bfl SegmentFirst      = 0x4000
    bfl SegmentContinue   = 0x0000
    bfl SegmentLast       = 0x8000
    bfl SegmentStandalone = 0xC000



{-# INLINABLE unpackSeqFlags #-}
unpackSeqFlags :: Word16 -> (SegmentationFlags, SSC)
unpackSeqFlags seg = (fl, mkSSC (seg .&. 0x3FFF))
  where
    fl = case seg .&. 0xC000 of
        0x4000 -> SegmentFirst
        0x0000 -> SegmentContinue
        0x8000 -> SegmentLast
        0xC000 -> SegmentStandalone
        _      -> SegmentStandalone


-- | decode a packet. The CucEncoding and the HasCRC is only used for C&C Packets
{-# INLINABLE decodePktMissionSpecific #-}
decodePktMissionSpecific
    :: ByteString
    -> PUSMissionSpecific
    -> CucEncoding
    -> HasCRC
    -> ProtocolInterface
    -> Either Text (ProtocolPacket PUSPacket)
decodePktMissionSpecific pkt missionSpecific timeEncoding crcFlag commIF
    | B.length pkt < fixedSizeOf @PUSHeader + fixedSizeOf @CRC = Left
        "PUS Packet is too short"
    | otherwise = case crcCheck pkt of
        Left  err -> Left err
        Right (result, _pl, extractedCRC, calcdCRC) -> if result
            then
                case
                    A.parse
                        (pusPktParser missionSpecific timeEncoding crcFlag commIF
                        )
                        pkt
                of
                    A.Fail _ _ err -> Left (T.pack err)
                    A.Partial f    -> case f B.empty of
                        A.Fail _ _ err -> Left (T.pack err)
                        A.Partial _ ->
                            Left
                                "PUS Packet: not enough input to parse PUS Packet"
                        A.Done _ p -> Right p
                    A.Done _ p -> Right p
            else
                Left
                $  T.run
                $  T.text "CRC Error: received: "
                <> T.string (show extractedCRC)
                <> T.text " calculated: "
                <> T.string (show calcdCRC)


-- | Parse a PUS packet. The CucEncoding and the HasCRC is only used for C&C Packets
pusPktParser
    :: PUSMissionSpecific
    -> CucEncoding
    -> HasCRC
    -> ProtocolInterface
    -> Parser (ProtocolPacket PUSPacket)
pusPktParser missionSpecific timeEncoding crcFlag comm = do
    hdr <- pusPktHdrParser
    --traceM $ "pusPktParser: pusHdr = " <> T.pack (show hdr)
    pusPktParserPayload missionSpecific timeEncoding crcFlag comm hdr


pusPktParserPayload
    :: PUSMissionSpecific
    -> CucEncoding
    -> HasCRC
    -> ProtocolInterface
    -> PUSHeader
    -> Parser (ProtocolPacket PUSPacket)
pusPktParserPayload missionSpecific timeEncoding crcFlag comm@(IfCnc _) hdr = do
    case _pusHdrTcVersion hdr of
        3 -> cncPusPktParserPayload timeEncoding crcFlag comm hdr
        _ -> pusPktParserPayloadSrc missionSpecific comm hdr
pusPktParserPayload missionSpecific _timeEncoding _crcFlag comm hdr = do
    pusPktParserPayloadSrc missionSpecific comm hdr


pusPktParserPayloadSrc
    :: PUSMissionSpecific
    -> ProtocolInterface
    -> PUSHeader
    -> Parser (ProtocolPacket PUSPacket)
pusPktParserPayloadSrc missionSpecific comm hdr = do
    (dfh, crcFlag) <- if
        | isNctrs comm
        -> if hdr ^. pusHdrDfhFlag
            then case hdr ^. pusHdrType of
                PUSTM -> (, True)
                    <$> dfhParser (missionSpecific ^. pmsTMDataFieldHeader)
                PUSTC -> (, True)
                    <$> dfhParser (missionSpecific ^. pmsTCDataFieldHeader)
            else return (PUSEmptyHeader, True)
        | isNdiu comm
        -> if hdr ^. pusHdrDfhFlag
            then case hdr ^. pusHdrType of
                PUSTM -> (, True)
                    <$> dfhParser (missionSpecific ^. pmsTMDataFieldHeader)
                PUSTC -> (, True)
                    <$> dfhParser (missionSpecific ^. pmsTCDataFieldHeader)
            else return (PUSEmptyHeader, True)
        | isSLE comm
        -> if hdr ^. pusHdrDfhFlag
            then case hdr ^. pusHdrType of
                PUSTM -> (, True)
                    <$> dfhParser (missionSpecific ^. pmsTMDataFieldHeader)
                PUSTC -> (, True)
                    <$> dfhParser (missionSpecific ^. pmsTCDataFieldHeader)
            else return (PUSEmptyHeader, True)
        | isCnc comm
        -> if hdr ^. pusHdrDfhFlag
            then case hdr ^. pusHdrType of
-- FIXME CRC handling maybe needs to be configured via MIB or config
                PUSTM -> (, False)
                    <$> dfhParser (missionSpecific ^. pmsTMDataFieldHeader)
                PUSTC -> (, False)
                    <$> dfhParser (missionSpecific ^. pmsTCDataFieldHeader)
            else if hdr ^. pusHdrTcVersion == 3
                then return (PUSEmptyHeader, False)
                else return (PUSEmptyHeader, True)
        | isEden comm
        -> if hdr ^. pusHdrDfhFlag
            then case hdr ^. pusHdrType of
                PUSTM -> (, True)
                    <$> dfhParser (missionSpecific ^. pmsTMDataFieldHeader)
                PUSTC -> (, True)
                    <$> dfhParser (missionSpecific ^. pmsTCDataFieldHeader)
            else if hdr ^. pusHdrTcVersion == 3
                then return (PUSEmptyHeader, False)
                else return (PUSEmptyHeader, True)
        | otherwise
        -> fail $ "Unknown protocol type: " <> show comm

    --traceShowM dfh

    -- The length in the PUS header is data length - 1, so we need to take
    -- one byte more, but we have to ignore the CRC, which is also considered
    -- in the length calculation

    dat <- A.take (fromIntegral (hdr ^. pusHdrTcLength + 1) - dfhLength dfh)

    let pl = case comm of
            IfCnc _ -> case dfh of
                PUSCnCTCHeader { _cncTcCrcFlags = val } -> if val == 1      -- the packet contains a CRC
                    then B.take (B.length dat - crcLen) dat
                    else dat
                _ -> dat
            _ -> B.take (B.length dat - crcLen) dat

    --traceM (hexdumpBS pl)

    return
        (ProtocolPacket comm (PUSPacket hdr dfh Nothing (HexBytes pl) crcFlag))


cncPusPktParserPayload
    :: CucEncoding
    -> HasCRC
    -> ProtocolInterface
    -> PUSHeader
    -> Parser (ProtocolPacket PUSPacket)
cncPusPktParserPayload timeEncoding cncCRC comm hdr = do
    let dfhTemplate = cncTMHeader timeEncoding

    (dfh, crcFlag) <- if hdr ^. pusHdrDfhFlag
        then case hdr ^. pusHdrType of
            PUSTM -> (, hasCRC cncCRC) <$> dfhParser dfhTemplate
            PUSTC -> (, hasCRC cncCRC) <$> dfhParser defaultCnCTCHeader
        else if hdr ^. pusHdrTcVersion == 3
            then return (PUSEmptyHeader, False)
            else return (PUSEmptyHeader, True)

    dat <- A.take (fromIntegral (hdr ^. pusHdrTcLength + 1) - dfhLength dfh)

    let
        pl = case comm of
            IfCnc _ -> case dfh of
                PUSCnCTCHeader { _cncTcCrcFlags = val } ->
                    if (val == 1) || crcFlag      -- the packet contains a CRC
                        then B.take (B.length dat - crcLen) dat
                        else dat
                PUSCnCTMHeader{} -> if crcFlag
                    then B.take (B.length dat - crcLen) dat
                    else dat
                _ -> dat
            _ -> B.take (B.length dat - crcLen) dat

    return
        (ProtocolPacket comm (PUSPacket hdr dfh Nothing (HexBytes pl) crcFlag))
