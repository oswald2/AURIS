{-# LANGUAGE
    DeriveGeneric
    , GeneralizedNewtypeDeriving
    , NoImplicitPrelude
    , TemplateHaskell
#-}
module Data.PUS.PUSPacket
    ( PUSPacket
    , encodePUSPacket
    , pusHdr
    , pusDfh
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
    )
where


import           RIO

--import           GHC.Generics

import           Control.Lens                   ( makeLenses )

import           Data.Binary
import           Data.Aeson

--import           Data.PUS.Types
import           Data.PUS.APID
import           Data.PUS.SegmentationFlags
import           Data.PUS.PUSDfh





data PUSPacketType = PUSTM | PUSTC deriving (Ord, Eq, Enum, Show, Read, Generic)


instance Binary PUSPacketType
instance FromJSON PUSPacketType
instance ToJSON PUSPacketType where
    toEncoding = genericToEncoding defaultOptions


data PUSHeader = PUSHeader {
    _pusHdrPktID :: Word16,
    _pusHdrTcVersion :: Word8,
    _pusHdrType ::  PUSPacketType,
    _pusHdrDfh :: Bool,
    _pusHdrTcApid :: APID,
    _pusHdrSeqFlags :: SegmentationFlags,
    _pusHdrTcSsc :: Word16,
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
    _pusDfh :: DFH,
    -- _pusPIs :: Maybe (TMPIVal, TMPIVal),
    _pusData :: ByteString
    } deriving (Show, Generic)

makeLenses ''PUSPacket

--instance Binary PUSPacket
--instance FromJSON PUSPacket
-- instance ToJSON PUSPacket where
--     toEncoding = genericToEncoding defaultOptions



encodePUSPacket :: PUSPacket -> ByteString
encodePUSPacket _ = undefined
