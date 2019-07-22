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
    , encodePUSPacket
    , encodePUSPktChoice
    , decodePktMissionSpecific
    , pusHdr
    , pusDfh
    , pusPIs
    , pusData
    , pusHdrPktID
    , pusHdrTcVersion
    , pusHdrType
    , pusHdrDfhFlag
    , pusHdrTcApid
    , pusHdrSeqFlags
    , pusHdrTcSsc
    , pusHdrSeqCtrl
    , pusHdrTcLength
    , tmpiValue
    , tmpiOffset
    , tmpiWidth
    , pusPktHdrBuilder
    , pusPktHdrParser
    , pusPktParser
    )
where


import           RIO                     hiding ( Builder )
import qualified RIO.ByteString                as B
import qualified RIO.Vector.Storable           as V
import qualified RIO.Vector.Storable.Unsafe    as V
                                                ( unsafeFreeze )
import           Data.Vector.Storable.ByteString
import qualified RIO.Text                      as T

import qualified Text.Builder                  as T
import           Control.Lens                   ( makeLenses
                                                , (.~)
                                                )
import           Control.Monad.ST

import           Data.Binary
import           Data.Aeson
import           Codec.Serialise
import           Data.Attoparsec.ByteString     ( Parser )
import qualified Data.Attoparsec.ByteString    as A
import qualified Data.Attoparsec.Binary        as A
import           ByteString.StrictBuilder
import           Data.Bits

import           Data.PUS.Types
import           Data.PUS.APID
import           Data.PUS.SegmentationFlags
import           Data.PUS.PUSDfh
import           Data.PUS.CRC
import           Data.PUS.MissionSpecific.Definitions

import           Protocol.ProtocolInterfaces
import           Protocol.SizeOf

import           General.SetBitField
import           General.Types



data PUSPacketType = PUSTM | PUSTC deriving (Ord, Eq, Enum, Show, Read, Generic)


instance Binary PUSPacketType
instance Serialise PUSPacketType
instance FromJSON PUSPacketType
instance ToJSON PUSPacketType where
    toEncoding = genericToEncoding defaultOptions


data TMPIVal = TMPIVal {
    _tmpiValue :: !Int64,
    _tmpiOffset :: !ByteOffset,
    _tmpiWidth :: !Word16
    } deriving (Eq, Show, Read)
makeLenses ''TMPIVal


data PUSHeader = PUSHeader {
    _pusHdrPktID :: !Word16,
    _pusHdrTcVersion :: !Word8,
    _pusHdrType ::  !PUSPacketType,
    _pusHdrDfhFlag :: !Bool,
    _pusHdrTcApid :: !APID,
    _pusHdrSeqFlags :: !SegmentationFlags,
    _pusHdrTcSsc :: !SSC,
    _pusHdrSeqCtrl :: !Word16,
    _pusHdrTcLength :: !Word16
    } deriving(Show, Read, Generic)

makeLenses ''PUSHeader

instance Binary PUSHeader
instance Serialise PUSHeader
instance FromJSON PUSHeader
instance ToJSON PUSHeader where
    toEncoding = genericToEncoding defaultOptions

instance FixedSize PUSHeader where
    fixedSizeOf = 6


data PUSPacket = PUSPacket {
    _pusHdr :: !PUSHeader,
    _pusDfh :: !DataFieldHeader,
    _pusPIs :: Maybe (TMPIVal, TMPIVal),
    _pusData :: !ByteString
    } deriving (Show, Generic)
makeLenses ''PUSPacket

instance SizeOf PUSPacket where
    sizeof x =
        fixedSizeOf @PUSHeader + sizeof (_pusDfh x) + B.length (_pusData x)





-- | encodes a packet and sets the PI1/2 values for correct identification
{-# INLINABLE encodePUSPacket #-}
encodePUSPacket :: PUSPacket -> ByteString
encodePUSPacket pkt =
    let !encPkt = encodePktWithoutCRC pkt True in crcEncodeAndAppendBS encPkt

-- | encodes a packet and sets the PI1/2 values for correct identification
{-# INLINABLE encodePUSPktChoice #-}
encodePUSPktChoice :: Bool -> PUSPacket -> ByteString
encodePUSPktChoice True pkt =
    let !encPkt = encodePktWithoutCRC pkt True in crcEncodeAndAppendBS encPkt
encodePUSPktChoice False pkt =
    let !encPkt = encodePktWithoutCRC pkt False in encPkt



-- | encodes' a packet, but does not calculate and append the CRC. The flag
-- | @useCRC is given for the case there is further processing of the packet
-- | before adding the CRC. Actually, if the flag is True, the length will be
-- | calculated differently, as the CRC is omitted.
-- | So if you need to append the CRC later, this flag should be True, otherwise
-- | False
{-# INLINABLE encodePktWithoutCRC #-}
encodePktWithoutCRC :: PUSPacket -> Bool -> ByteString
encodePktWithoutCRC pkt useCRC =
    let newPkt  = pusPktUpdateLen pkt useCRC
        encHdr  = pusPktHdrBuilder (newPkt ^. pusHdr)
        encDfh  = dfhBuilder (newPkt ^. pusDfh)
        payload = newPkt ^. pusData
        !pl     = builderBytes $ if newPkt ^. pusHdr . pusHdrDfhFlag 
            then encHdr <> encDfh <> bytes payload
            else encHdr <> bytes payload
    in  applied pkt pl
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
            dfhLen + B.length (_pusData pkt) + if useCRC then crcLen else 0
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
                             (fromIntegral wid1)
                             (fromIntegral val1)
            else return v

        if off2 >= 0
            then setBitFieldR v1
                             (toBitOffset off2)
                             (fromIntegral wid2)
                             (fromIntegral val2)
            else return v1



{-# INLINABLE pusPktHdrBuilder #-}
pusPktHdrBuilder :: PUSHeader -> Builder
pusPktHdrBuilder hdr =
    let !pktId = packPktID (_pusHdrTcVersion hdr)
                           (_pusHdrType hdr)
                           (_pusHdrDfhFlag hdr)
                           (_pusHdrTcApid hdr)
        !seqFlags = packSeqFlags (_pusHdrSeqFlags hdr) (_pusHdrTcSsc hdr)
    in  word16BE pktId <> word16BE seqFlags <> word16BE (_pusHdrTcLength hdr)



{-# INLINABLE pusPktHdrParser #-}
pusPktHdrParser :: Parser PUSHeader
pusPktHdrParser = do
    pktId    <- A.anyWord16be
    seqFlags <- A.anyWord16be
    len      <- A.anyWord16be

    let (!vers, !tp, !dfh, !apid) = unpackPktID pktId
        (!sf, !ssc)               = unpackSeqFlags seqFlags

    return (PUSHeader pktId vers tp dfh apid sf ssc seqFlags len)



{-# INLINABLE packPktID #-}
packPktID :: Word8 -> PUSPacketType -> Bool -> APID -> Word16
packPktID !vers !tp !dfh (APID apid) =
    let versShifted = fromIntegral vers `shiftL` 13
        typeShifted = case tp of
            PUSTM -> 0x0000
            PUSTC -> 0x1000
        dfhShifted = if dfh 
            then 0x0800
            else 0x0000
        apidMasked = apid .&. 0x7ff
    in  versShifted .|. typeShifted .|. dfhShifted .|. apidMasked



{-# INLINABLE unpackPktID #-}
unpackPktID :: Word16 -> (Word8, PUSPacketType, Bool, APID)
unpackPktID pktID =
    let !apid = pktID .&. 0x7ff
        !dfh  = (pktID .&. 0x0800) /= 0
        !tp   = if (pktID .&. 0x1000) /= 0 
            then PUSTC
            else PUSTM
        !ver = (pktID .&. 0xe000) `shiftR` 13
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



{-# INLINABLE decodePktMissionSpecific #-}
decodePktMissionSpecific
    :: ByteString
    -> PUSMissionSpecific
    -> ProtocolInterface
    -> Either Text (ProtocolPacket PUSPacket)
decodePktMissionSpecific pkt missionSpecific commIF
    | B.length pkt < fixedSizeOf @PUSHeader + fixedSizeOf @CRC = Left
        "PUS Packet is too short"
    | otherwise = case crcCheck pkt of
        Left  err -> Left err
        Right (result, pl, extractedCRC, calcdCRC) -> if result
            then case A.parse (pusPktParser missionSpecific commIF) pl of
                A.Fail _ _ err -> Left (T.pack err)
                A.Partial _ ->
                    Left "PUS Packet: not enough input to parse PUS Packet"
                A.Done _ p -> Right p
            else
                Left
                $  T.run
                $  T.text "CRC Error: received: "
                <> T.string (show extractedCRC)
                <> T.text " calculated: "
                <> T.string (show calcdCRC)



pusPktParser
    :: PUSMissionSpecific
    -> ProtocolInterface
    -> Parser (ProtocolPacket PUSPacket)
pusPktParser missionSpecific comm = do
    hdr <- pusPktHdrParser
    dfh <- if
        | comm == IF_NCTRS -> if hdr ^. pusHdrDfhFlag
            then case hdr ^. pusHdrType of
                PUSTM -> dfhParser (missionSpecific ^. pmsTMDataFieldHeader)
                PUSTC -> dfhParser (missionSpecific ^. pmsTCDataFieldHeader)
            else return PUSEmptyHeader
        | comm == IF_CNC -> if hdr ^. pusHdrDfhFlag
            then dfhParser defaultCnCTCHeader
            else return PUSEmptyHeader
        | comm == IF_EDEN || comm == IF_EDEN_SCOE -> if hdr ^. pusHdrDfhFlag
            then case hdr ^. pusHdrType of
                PUSTM -> dfhParser (missionSpecific ^. pmsTMDataFieldHeader)
                PUSTC -> dfhParser (missionSpecific ^. pmsTCDataFieldHeader)
            else return PUSEmptyHeader
        | otherwise -> fail $ "Unknown protocol type: " <> show comm
    dat <- A.take (fromIntegral (hdr ^. pusHdrTcLength) + 1)

    let pl = case comm of
            IF_CNC -> case dfh of
                PUSCnCTCHeader { _cncTcCrcFlags = val } -> if val == 1      -- the packet contains a CRC
                    then B.take (B.length dat - crcLen) dat
                    else dat
                _ -> dat
            _ -> dat

    return (ProtocolPacket comm (PUSPacket hdr dfh Nothing pl))
