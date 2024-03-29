{-# LANGUAGE DeriveAnyClass #-}
module Data.PUS.ISL
    ( ISL(..)
    , islParser
    , islBuilder
    , islPacketParser
    , ISLEncPktHdr(..)
    , defaultIslEncPktHdr
    , islEncPktHdr
    , encodeISL
    , isIslPktID
    , islEncHdrBuilder
    , islEncHdrParser
    , islHeaderBuilder
    , islHeaderParser
    , islHeaderSize
    , ISLType(..)
    , ISLRouting(..)
    , ISLHeader(..)
    ) where

import           RIO                     hiding ( Builder )
import qualified RIO.ByteString                as BS

import           ByteString.StrictBuilder      as B

import           Codec.Serialise
import           Data.Aeson
import           Data.Attoparsec.Binary        as A
import           Data.Attoparsec.ByteString    as A
import           Data.Bits

import           Data.PUS.EncTime               ( CucEncoding(..) )
import           Data.PUS.MissionSpecific.Definitions
import           Data.PUS.PUSPacket

import           Protocol.ProtocolInterfaces

import           General.PUSTypes
import           General.SizeOf



encodeISL :: ISLHeader -> ByteString -> ByteString
encodeISL hdr payload =
    builderBytes
        $ let
              len =
                  BS.length payload
                      + fixedSizeOf @ISLHeader
                      + fixedSizeOf @ISLEncPktHdr
              encHdr = ISLEncPktHdr 0 (fromIntegral len)
          in
              islEncHdrBuilder encHdr <> islHeaderBuilder hdr <> bytes payload

data ISLEncPktHdr = ISLEncPktHdr
    { islID     :: !Word16
    , islLength :: !Word16
    }
    deriving stock (Read, Show, Generic)
    deriving anyclass (FromJSON, ToJSON, Serialise, NFData)

defaultIslEncPktHdr :: ISLEncPktHdr
defaultIslEncPktHdr = ISLEncPktHdr { islID = islPktID, islLength = 0 }

islEncPktHdr :: Word16 -> ISLEncPktHdr
islEncPktHdr len = ISLEncPktHdr { islID = islPktID, islLength = len }

islPktID :: Word16
islPktID = 0b111_111_10_0000_0000

{-# INLINABLE isIslPktID #-}
isIslPktID :: Word16 -> Bool
isIslPktID val = val == islPktID

instance FixedSize ISLEncPktHdr where
    fixedSizeOf = 4

islEncHdrBuilder :: ISLEncPktHdr -> B.Builder
islEncHdrBuilder ISLEncPktHdr {..} = word16BE islPktID <> word16BE islLength

islEncHdrParser :: A.Parser ISLEncPktHdr
islEncHdrParser = do
    i   <- A.anyWord16be
    len <- A.anyWord16be
    return ISLEncPktHdr { islID = i, islLength = len }


data ISLType =
    IslATC
    | IslFTM
    | IslWildcard
    | IslSpare
    deriving stock (Read, Show, Generic)
    deriving anyclass (FromJSON, ToJSON, Serialise, NFData)

{-# INLINABLE wordToIslType #-}
wordToIslType :: Word16 -> ISLType
wordToIslType val = case val .&. 0b1100_0000_0000_0000 of
    0b0000_0000_0000_0000 -> IslATC
    0b0100_0000_0000_0000 -> IslFTM
    0b1000_0000_0000_0000 -> IslWildcard
    0b1100_0000_0000_0000 -> IslSpare
    _                     -> IslSpare

islTypeToWord :: ISLType -> Word16
islTypeToWord IslATC      = 0b0000_0000_0000_0000
islTypeToWord IslFTM      = 0b0100_0000_0000_0000
islTypeToWord IslWildcard = 0b1000_0000_0000_0000
islTypeToWord IslSpare    = 0b1100_0000_0000_0000


data ISLRouting =
  ISLSingleHop
  | ISLMultiHopFlooding
  | ISLMultiHopRealTimeRing
  | ISLHopSpare
    deriving stock (Read, Show, Generic)
    deriving anyclass (FromJSON, ToJSON, Serialise, NFData)


data ISLHeader = ISLHeader
    { islType    :: !ISLType
    , islSCID    :: !SCID
    , islTTL     :: !Word8
    , islRouting :: !ISLRouting
    , islIsDummy :: !Bool
    }
    deriving stock (Read, Show, Generic)
    deriving anyclass (FromJSON, ToJSON, Serialise, NFData)


instance FixedSize ISLHeader where
    fixedSizeOf = 3

{-# INLINABLE islHeaderParser #-}
islHeaderParser :: Parser ISLHeader
islHeaderParser = do
    w <- anyWord16be
    b <- anyWord8
    return (convertToILSHeader w b)



{-# INLINABLE islHeaderBuilder #-}
islHeaderBuilder :: ISLHeader -> B.Builder
islHeaderBuilder hdr =
    let (w, b) = convertISLHeader hdr in word16BE w <> B.word8 b


convertISLHeader :: ISLHeader -> (Word16, Word8)
convertISLHeader ISLHeader {..} =
    let !w =
            islTypeToWord islType
                .|. ((getSCID islSCID) `shiftL` 4)
                .|. fromIntegral ((ttl `shiftR` 3) .&. 0x0F)
        !b  = (ttl `shiftL` 5) .|. (routing islRouting) .|. dummy islIsDummy
        ttl = islTTL .&. 0x7F

        routing ISLSingleHop            = 0b000_00000
        routing ISLMultiHopFlooding     = 0b001_00000
        routing ISLMultiHopRealTimeRing = 0b010_00000
        routing ISLHopSpare             = 0b011_00000

        dummy True  = 0b0000_0010
        dummy False = 0b0000_0010
    in  (w, b)

convertToILSHeader :: Word16 -> Word8 -> ISLHeader
convertToILSHeader w b =
    let
        t    = wordToIslType w
        scid = mkSCID $ (w .&. 0b0011_1111_1111_0000) `shiftR` 4
        ttl  = (w .&. 0x000F `shiftL` 3)
            .|. fromIntegral ((b .&. 0b1110_0000) `shiftR` 5)

        routing = case b .&. 0b0001_1100 of
            0b0000_0000 -> ISLSingleHop
            0b0000_0100 -> ISLMultiHopFlooding
            0b0000_1000 -> ISLMultiHopRealTimeRing
            0b0000_1100 -> ISLHopSpare
            _           -> ISLHopSpare

        dummy = (b .&. 0b0000_0010) /= 0
    in
        ISLHeader { islType    = t
                  , islSCID    = scid
                  , islTTL     = fromIntegral ttl
                  , islRouting = routing
                  , islIsDummy = dummy
                  }

data ISL = ISL
    { islEncHdr :: !ISLEncPktHdr
    , islHdr    :: !ISLHeader
    , islData   :: !ByteString
    }
    deriving (Read, Show)

{-# INLINABLE islHeaderSize #-}
islHeaderSize :: Int
islHeaderSize = fixedSizeOf @ISLEncPktHdr + fixedSizeOf @ISLHeader

islParser :: Parser ISL
islParser = do
    eh  <- islEncHdrParser
    hdr <- islHeaderParser
    let len =
            fromIntegral (islLength eh)
                - fixedSizeOf @ISLHeader
                - fixedSizeOf @ISLEncPktHdr
    dat <- A.take len
    return ISL { islEncHdr = eh, islHdr = hdr, islData = dat }

islBuilder :: ISL -> Builder
islBuilder isl =
    let hdr    = islEncHdr isl
        newHdr = hdr
            { islLength = fromIntegral
                              ( BS.length (islData isl)
                              + fixedSizeOf @ISLHeader
                              + fixedSizeOf @ISLEncPktHdr
                              )
            }
    in  islEncHdrBuilder newHdr <> islHeaderBuilder (islHdr isl) <> bytes
            (islData isl)


-- | Parsed a PUS Packet and returns the parsed packet as well as a boolean
-- specifying if this was an ISL packet (True) or not (False).
islPacketParser
    :: PUSMissionSpecific
    -> CucEncoding
    -> HasCRC
    -> ProtocolInterface
    -> Parser (ProtocolPacket PUSPacket, Bool)
islPacketParser missionSpecific timeEncoding hasCRC interf = do
    pktID <- A.anyWord16be
    if pktID == islPktID
        then do
            void $ A.anyWord16be
            -- skip ISL header
            void $ A.take (fixedSizeOf @ISLHeader)
            pkt <- pusPktParser missionSpecific timeEncoding hasCRC interf
            return (pkt, True)
        else do
            hdr <- pusPktHdrParserWithoutPktID pktID
            pkt <- pusPktParserPayload missionSpecific
                                       timeEncoding
                                       hasCRC
                                       interf
                                       hdr
            return (pkt, False)
