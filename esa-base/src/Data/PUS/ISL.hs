{-# LANGUAGE DeriveAnyClass #-}
module Data.PUS.ISL
    ( ISLEncPktHdr(..)
    , encodeISL
    , isIslPktID
    , islEncHdrBuilder
    , islEncHdrParser
    , islHeaderBuilder
    , islHeaderParser
    , ISLType(..)
    , ISLRouting(..)
    , ISLHeader(..)
    ) where

import           RIO
import qualified RIO.ByteString                as BS

import           ByteString.StrictBuilder      as B

import           Codec.Serialise
import           Data.Aeson
import           Data.Attoparsec.Binary        as A
import           Data.Attoparsec.ByteString    as A
import           Data.Bits

import           General.PUSTypes
import           General.SizeOf



encodeISL :: ISLHeader -> ByteString -> ByteString
encodeISL hdr payload =
    builderBytes
        $ let len    = BS.length payload + fixedSizeOf @ISLHeader
              encHdr = ISLEncPktHdr 0 (fromIntegral len)
          in  islEncHdrBuilder encHdr <> islHeaderBuilder hdr <> bytes payload

data ISLEncPktHdr = ISLEncPktHdr
    { islID     :: !Word16
    , islLength :: !Word16
    }
    deriving stock (Read, Show, Generic)
    deriving anyclass (FromJSON, ToJSON, Serialise, NFData)


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

