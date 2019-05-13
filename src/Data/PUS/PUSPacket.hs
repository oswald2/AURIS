{-# LANGUAGE
    DeriveGeneric
    , GeneralizedNewtypeDeriving
    , NoImplicitPrelude
    , TemplateHaskell
    , BangPatterns
#-}
module Data.PUS.PUSPacket
    ( PUSPacket
    , encodePUSPacket
    , pusHdr
    , pusDfh
    , pusPIs
    , pusData
    , pusHdrPktID
    , pusHdrTcVersion
    , pusHdrType
    , pusHdrDfh
    , pusHdrTcApid
    , pusHdrSeqFlags
    , pusHdrTcSsc
    , pusHdrSeqCtrl
    , pusHdrTcLength
    , TMPIVal(..)
    , tmpiValue 
    , tmpiOffset
    , tmpiWidth 
    , pusPktHdrBuilder
    , pusPktHdrParser
    )
where


import           RIO hiding (Builder)

import           Control.Lens                   ( makeLenses )

import           Data.Binary
import           Data.Aeson
import qualified Data.Attoparsec.ByteString as A
import qualified Data.Attoparsec.Binary as A
import ByteString.StrictBuilder
import Data.Bits

import           Data.PUS.Types
import           Data.PUS.APID
import           Data.PUS.SegmentationFlags
import           Data.PUS.PUSDfh





data PUSPacketType = PUSTM | PUSTC deriving (Ord, Eq, Enum, Show, Read, Generic)


instance Binary PUSPacketType
instance FromJSON PUSPacketType
instance ToJSON PUSPacketType where
    toEncoding = genericToEncoding defaultOptions


data TMPIVal = TMPIVal {
    _tmpiValue :: Int,
    _tmpiOffset :: Int,
    _tmpiWidth :: Word16
    } deriving (Eq, Show, Read)
makeLenses ''TMPIVal


data PUSHeader = PUSHeader {
    _pusHdrPktID :: Word16,
    _pusHdrTcVersion :: Word8,
    _pusHdrType ::  PUSPacketType,
    _pusHdrDfh :: Bool,
    _pusHdrTcApid :: APID,
    _pusHdrSeqFlags :: SegmentationFlags,
    _pusHdrTcSsc :: SSC,
    _pusHdrSeqCtrl :: Word16,
    _pusHdrTcLength :: Word16
    } deriving(Show, Read, Generic)

makeLenses ''PUSHeader

instance Binary PUSHeader
instance FromJSON PUSHeader
instance ToJSON PUSHeader where
    toEncoding = genericToEncoding defaultOptions


data PUSPacket = PUSPacket {
    _pusHdr :: PUSHeader,
    _pusDfh :: DataFieldHeader,
    _pusPIs :: Maybe (TMPIVal, TMPIVal),
    _pusData :: ByteString
    } deriving (Show, Generic)
makeLenses ''PUSPacket


encodePUSPacket :: PUSPacket -> ByteString
encodePUSPacket _ = undefined


pusPktHdrBuilder :: PUSHeader -> Builder
pusPktHdrBuilder hdr
    = let
          !pktId = packPktID (_pusHdrTcVersion hdr)
                            (_pusHdrType hdr)
                            (_pusHdrDfh hdr)
                            (_pusHdrTcApid hdr)
          !seqFlags = packSeqFlags (_pusHdrSeqFlags hdr) (_pusHdrTcSsc hdr)
      in
          word16BE pktId <> word16BE seqFlags <> word16BE (_pusHdrTcLength hdr)


pusPktHdrParser :: A.Parser PUSHeader
pusPktHdrParser = do
    pktId    <- A.anyWord16be
    seqFlags <- A.anyWord16be
    len      <- A.anyWord16be

    let (!vers, !tp, !dfh, !apid) = unpackPktID pktId
        (!sf, !ssc)             = unpackSeqFlags seqFlags

    return (PUSHeader pktId vers tp dfh apid sf ssc seqFlags len)




packPktID :: Word8 -> PUSPacketType -> Bool -> APID -> Word16
packPktID !vers !tp !dfh !(APID apid) =
    let versShifted = fromIntegral vers `shiftL` 13
        typeShifted = case tp of
            PUSTM -> 0x0000
            PUSTC -> 0x1000
        dfhShifted = case dfh of
            True  -> 0x0800
            False -> 0x0000
        apidMasked = apid .&. 0x7ff
    in  versShifted .|. typeShifted .|. dfhShifted .|. apidMasked


unpackPktID :: Word16 -> (Word8, PUSPacketType, Bool, APID)
unpackPktID pktID =
    let !apid = pktID .&. 0x7ff
        !dfh  = (pktID .&. 0x0800) /= 0
        !tp   = case (pktID .&. 0x1000) /= 0 of
            True  -> PUSTC
            False -> PUSTM
        !ver = (pktID .&. 0xe000) `shiftR` 13
    in  (fromIntegral ver, tp, dfh, APID apid)



packSeqFlags :: SegmentationFlags -> SSC -> Word16
packSeqFlags !flags !ssc = bfl flags .|. (getSSC ssc) .&. 0x3FFF
    where
    bfl SegmentFirst      = 0x4000
    bfl SegmentContinue   = 0x0000
    bfl SegmentLast       = 0x8000
    bfl SegmentStandalone = 0xC000



unpackSeqFlags :: Word16 -> (SegmentationFlags, SSC)
unpackSeqFlags seg = (fl, mkSSC (seg .&. 0x3FFF))
    where
    fl = case seg .&. 0xC000 of
        0x4000 -> SegmentFirst
        0x0000 -> SegmentContinue
        0x8000 -> SegmentLast
        0xC000 -> SegmentStandalone
        _      -> SegmentStandalone


        