{-|
Module      : Data.PUS.TMFrame
Description : Data type for TM transfer frames
Copyright   : (c) Michael Oswald, 2019
License     : BSD-3
Maintainer  : michael.oswald@onikudaki.net
Stability   : experimental
Portability : POSIX

This module is about the 'TMFrame' data type. A TM frame is the transport
mechanism for TM packets stored in it's data part. Frames are identified by
it's spacecraft ID as well as virtual channel ID and virutal channel frame count
to check for correct sequences of TM frames.

TODO: implement TM Frame secondary headers

|-}
{-# LANGUAGE OverloadedStrings
    , BangPatterns
    , NoImplicitPrelude
    , DeriveGeneric
    , TypeApplications
    , MultiWayIf
    , TemplateHaskell
#-}
module Data.PUS.TMFrame
    ( TMFrameConfig(..)
    , defaultTMFrameConfig
    , TMFrameHeader(..)
    , TMFrameSecHeader(..)
    , TMFrame(..)
    , FirstHeaderPtrVal(..)
    , tmFrameVersion
    , tmFrameScID
    , tmFrameVcID
    , tmFrameOpControl
    , tmFrameMCFC
    , tmFrameVCFC
    , tmFrameDfh
    , tmFrameSync
    , tmFrameOrder
    , tmFrameSegID
    , tmFrameFirstHeaderPtr
    , tmFrameIdlePtr
    , tmFrameNoFirstHeader
    , tmSegmentLength
    , tmFrameHdr
    , tmFrameSecHdr
    , tmFrameData
    , tmFrameOCF
    , tmFrameFECW
    , tmFrameBuilder
    , tmFrameParser
    , makeTMFrame
    , tmFrameMaxDataLen
    , tmFrameMaxDataLenFlag
    , tmFrameMinLen
    , tmFrameGetPrevAndRest
    , decodeFrame
    , encodeFrame
    , tmFrameAppendCRC
    , tmFrameCheckOrder
    , tmFrameCheckSync
    , tmFrameCheckCRC
    , isIdleTmFrame
    , tmFrameFHType
    , tmFrameDefaultHeader
    , displayFHP
    , cfgFrameMaxSize
    ) where


import           RIO                     hiding ( Builder )
import qualified RIO.ByteString                as B

import           Control.Lens                   ( makeLenses )

import           Codec.Serialise
import           Codec.Serialise.Encoding       ( encodeWord16 )
import           Codec.Serialise.Decoding       ( decodeWord16 )
import           Conduit.PayloadParser
import           Data.Aeson
import qualified Data.Attoparsec.Binary        as A
import           Data.Attoparsec.ByteString     ( Parser )
import qualified Data.Attoparsec.ByteString    as A
import           Data.Bits
import           Data.ByteString.Base64.Type

import           ByteString.StrictBuilder

import           Data.PUS.CLCW
import           Data.PUS.CRC
import           Data.PUS.MissionSpecific.Definitions
import           Data.PUS.TMFrameDfh
import           General.PUSTypes

import           General.Hexdump
import           General.SizeOf

import           Refined


newtype FrameSize = FrameSize (Refined (And (Not (LessThan 128)) (Not (GreaterThan 2040))) Word16)
    deriving (Eq, Ord, Show, Read, ToJSON, FromJSON, NFData, Generic)

unFrameSize :: FrameSize -> Word16
unFrameSize (FrameSize x) = unrefine x

instance Serialise FrameSize where
    encode (FrameSize x) = encodeWord16 (unrefine x)
    decode = do
        x <- decodeWord16
        FrameSize <$> refineFail x


data TMFrameConfig = TMFrameConfig
    {
    -- | The maximum TM Frame length. This length is used in parsing
    -- the frame data, so it needs to be accurate. Default is by
    -- PUS Standard a value 1115 (1024 bytes data)
      cfgMaxTMFrameLen :: FrameSize
    -- | Indicates, if a TM frame does contain a CRC value
    , cfgTMFrameHasCRC :: Bool
    -- | The configured segment length for TM Frames
    , cfgTMSegLength   :: !TMSegmentLen
    }
    deriving (Eq, Show, Read, Generic)


defaultTMFrameConfig :: TMFrameConfig
defaultTMFrameConfig = TMFrameConfig { cfgMaxTMFrameLen = FrameSize $$(refineTH 1115)
                                     , cfgTMFrameHasCRC = True
                                     , cfgTMSegLength   = TMSegment65536
                                     }

cfgFrameMaxSize :: TMFrameConfig -> Word16 
cfgFrameMaxSize cfg = unFrameSize (cfgMaxTMFrameLen cfg)


instance FromJSON TMFrameConfig
instance ToJSON TMFrameConfig where
    toEncoding = genericToEncoding defaultOptions


data FirstHeaderPtrVal =
    FirstHeaderZero
    | FirstHeaderNoFH
    | FirstHeaderNonZero
    | FirstHeaderIdle
    deriving (Eq, Ord, Show, Read, Generic)

instance NFData FirstHeaderPtrVal
instance Serialise FirstHeaderPtrVal
instance FromJSON FirstHeaderPtrVal
instance ToJSON FirstHeaderPtrVal where
    toEncoding = genericToEncoding defaultOptions


-- | The primary frame header, adheres to the PUS Standard
data TMFrameHeader = TMFrameHeader
    { _tmFrameVersion        :: !Word8
    , _tmFrameScID           :: !SCID
    , _tmFrameVcID           :: !VCID
    , _tmFrameOpControl      :: !Bool
    , _tmFrameMCFC           :: !Word8
    , _tmFrameVCFC           :: !Word8
    , _tmFrameDfh            :: !Bool
    , _tmFrameSync           :: !Bool
    , _tmFrameOrder          :: !Bool
    , _tmFrameSegID          :: !TMSegmentLen
    , _tmFrameFirstHeaderPtr :: !Word16
    }
    deriving (Eq, Show, Read, Generic)
makeLenses ''TMFrameHeader

instance NFData TMFrameHeader
instance Serialise TMFrameHeader
instance FromJSON TMFrameHeader
instance ToJSON TMFrameHeader where
    toEncoding = genericToEncoding defaultOptions


data TMFrameSecHeader =
    TMFrameEmptySecHeader
    | TMFrameGAIASecHeader !Word32
    deriving (Eq, Show, Read, Generic)

instance NFData TMFrameSecHeader
instance Serialise TMFrameSecHeader
instance FromJSON TMFrameSecHeader
instance ToJSON TMFrameSecHeader where
    toEncoding = genericToEncoding defaultOptions

tmSecHdrLen :: TMFrameSecHeader -> Int
tmSecHdrLen TMFrameEmptySecHeader  = 0
tmSecHdrLen TMFrameGAIASecHeader{} = 4


instance Display TMFrameSecHeader where
    display TMFrameEmptySecHeader    = mempty
    display (TMFrameGAIASecHeader x) = "GAIA: vcfc_2=" <> display x



tmFrameDefaultHeader :: TMFrameHeader
tmFrameDefaultHeader = TMFrameHeader { _tmFrameVersion        = 0
                                     , _tmFrameScID           = mkSCID 0
                                     , _tmFrameVcID           = mkVCID 0
                                     , _tmFrameOpControl      = True
                                     , _tmFrameMCFC           = 0
                                     , _tmFrameVCFC           = 0
                                     , _tmFrameDfh            = False
                                     , _tmFrameSync           = True
                                     , _tmFrameOrder          = True
                                     , _tmFrameSegID          = TMSegment65536
                                     , _tmFrameFirstHeaderPtr = 0
                                     }


instance SizeOf TMFrameHeader where
    sizeof _ = 6

instance FixedSize TMFrameHeader where
    fixedSizeOf = 6


tmFrameIdlePtr :: Word16
tmFrameIdlePtr = 0x7FE

tmFrameNoFirstHeader :: Word16
tmFrameNoFirstHeader = 0x7FF


-- | The frame itself. It consists of a header, the data part and optionally
-- a CLCW (called OCF in TM terminology) and optionally a CRC value. Presence
-- of the CLCW is indicated in the header via the 'tmFrameOpControl' flag, the
-- presence of the CRC is indicated in the 'Config'
data TMFrame = TMFrame
    { _tmFrameHdr    :: !TMFrameHeader
    , _tmFrameSecHdr :: !TMFrameSecHeader
    , _tmFrameData   :: !ByteString
    , _tmFrameOCF    :: !(Maybe Word32)
    , _tmFrameFECW   :: !(Maybe CRC)
    }
    deriving (Eq, Show, Read, Generic)
makeLenses ''TMFrame

instance GetPayload TMFrame where
    getPayload = _tmFrameData

instance Display TMFrame where
    display TMFrame {..} =
        displayShow _tmFrameHdr
            <> display ("\nData:\n" :: Text)
            <> display (hexdumpBS _tmFrameData)
            <> display ("\n" :: Text)
            <> displayShow _tmFrameOCF
            <> displayShow _tmFrameFECW


instance NFData TMFrame
instance Serialise TMFrame
instance FromJSON TMFrame where
    parseJSON = withObject "TMFrame" $ \v ->
        TMFrame
            <$> v
            .:  "tmFrameHdr"
            <*> v
            .:  "tmFrameSecHdr"
            <*> (getByteString64 <$> v .: "tmFrameData")
            <*> v
            .:  "tmFrameOCF"
            <*> v
            .:  "tmFrameFECW"


instance ToJSON TMFrame where
    toJSON r = object
        [ "tmFrameHdr" .= _tmFrameHdr r
        , "tmFrameSecHdr" .= _tmFrameSecHdr r
        , "tmFrameData" .= makeByteString64 (_tmFrameData r)
        , "tmFrameOCF" .= _tmFrameOCF r
        , "tmFrameFECW" .= _tmFrameFECW r
        ]
    toEncoding r = pairs
        (  "tmFrameHdr"
        .= _tmFrameHdr r
        <> "tmFrameSecHdr"
        .= _tmFrameSecHdr r
        <> "tmFrameData"
        .= makeByteString64 (_tmFrameData r)
        <> "tmFrameOCF"
        .= _tmFrameOCF r
        <> "tmFrameFECW"
        .= _tmFrameFECW r
        )



{-# INLINABLE tmFrameFHType #-}
tmFrameFHType :: TMFrame -> FirstHeaderPtrVal
tmFrameFHType frame =
    let fh = frame ^. tmFrameHdr . tmFrameFirstHeaderPtr
    in  if
            | fh == 0                    -> FirstHeaderZero
            | fh == tmFrameNoFirstHeader -> FirstHeaderNoFH
            | fh == tmFrameIdlePtr       -> FirstHeaderIdle
            | otherwise                  -> FirstHeaderNonZero

displayFHP :: TMFrame -> Text
displayFHP frame =
    let fhp = frame ^. tmFrameHdr . tmFrameFirstHeaderPtr
    in  if
            | fhp == tmFrameIdlePtr       -> "IDLE"
            | fhp == tmFrameNoFirstHeader -> "NO FHP"
            | otherwise                   -> textDisplay fhp


{-# INLINABLE tmFrameCheckOrder #-}
tmFrameCheckOrder :: TMFrame -> Either Text ()
tmFrameCheckOrder frame = case tmFrameCheckSync frame of
    Left  err -> Left err
    Right ()  -> if frame ^. tmFrameHdr . tmFrameOrder
        then Left "PUS Standard only allows forward order of TM Frames"
        else Right ()

{-# INLINABLE tmFrameCheckSync #-}
tmFrameCheckSync :: TMFrame -> Either Text ()
tmFrameCheckSync frame = if frame ^. tmFrameHdr . tmFrameSync
    then Left "TM Frame Sync Bit is 1, currently not implemented."
    else Right ()

{-# INLINABLE isIdleTmFrame #-}
isIdleTmFrame :: TMFrame -> Bool
isIdleTmFrame frame =
    frame ^. tmFrameHdr . tmFrameFirstHeaderPtr == tmFrameIdlePtr


{-# INLINABLE tmFrameBuilder #-}
tmFrameBuilder :: TMFrame -> Builder
tmFrameBuilder f =
    tmFrameHeaderBuilder (_tmFrameHdr f) <> bytes (_tmFrameData f) <> maybe
        mempty
        word32BE
        (_tmFrameOCF f)



{-# INLINABLE makeTMFrame #-}
makeTMFrame
    :: Maybe Word32
    -> TMFrameHeader
    -> TMFrameSecHeader
    -> ByteString
    -> TMFrame
makeTMFrame clcw hdr secHdr pl = TMFrame hdr secHdr pl clcw Nothing


{-# INLINABLE tmFrameMaxDataLen #-}
tmFrameMaxDataLen
    :: TMFrameConfig -> PUSMissionSpecific -> TMFrameHeader -> Int
tmFrameMaxDataLen cfg missionSpecific hdr =
    tmFrameMaxDataLenFlag cfg missionSpecific (_tmFrameOpControl hdr)


{-# INLINABLE tmFrameMaxDataLenFlag #-}
tmFrameMaxDataLenFlag :: TMFrameConfig -> PUSMissionSpecific -> Bool -> Int
tmFrameMaxDataLenFlag cfg missionSpecific useCLCW =
    fromIntegral (unFrameSize (cfgMaxTMFrameLen cfg))
        - fixedSizeOf @TMFrameHeader
        - dfhLen
        - opLen
        - fecLen
  where
    opLen  = if useCLCW then fixedSizeOf @CLCW else 0
    fecLen = if cfgTMFrameHasCRC cfg then fixedSizeOf @CRC else 0
    dfhLen =
        maybe 0 tmFrameDfhLength (missionSpecific ^. pmsTMFrameDataFieldHeader)


{-# INLINABLE tmFrameMaxPayloadLen #-}
tmFrameMaxPayloadLen :: TMFrameConfig -> TMFrameHeader -> Int
tmFrameMaxPayloadLen conf _ =
    fromIntegral (unFrameSize (cfgMaxTMFrameLen conf)) - fixedSizeOf @TMFrameHeader


{-# INLINABLE tmFrameMinLen #-}
tmFrameMinLen :: Int
tmFrameMinLen = fixedSizeOf @TMFrameHeader + 1


{-# INLINABLE packFlags #-}
packFlags :: TMFrameHeader -> Word16
packFlags hdr =
    let
        !vers = fromIntegral (_tmFrameVersion hdr) .&. 0x03
        !scid = getSCID (_tmFrameScID hdr) .&. 0x03FF
        !vcid = fromIntegral (getVCID (_tmFrameVcID hdr))
        !opc  = if _tmFrameOpControl hdr then 1 else 0
        !result =
            (vers `shiftL` 14)
                .|. (scid `shiftL` 4)
                .|. (vcid `shiftL` 1)
                .|. opc
    in
        result

{-# INLINABLE unpackFlags #-}
unpackFlags :: Word16 -> (Word8, SCID, VCID, Bool)
unpackFlags i = (vers, scid, vcid, opc)
  where
    !vers = fromIntegral ((i `shiftR` 14) .&. 0x0003)
    !scid = mkSCID $ (i `shiftR` 4) .&. 0x03FF
    !vcid = fromIntegral ((i `shiftR` 1) .&. 0x07)
    !opc  = (i .&. 0x01) == 1


{-# INLINABLE packDFS #-}
packDFS :: TMFrameHeader -> Word16
packDFS hdr =
    let !dfh   = if _tmFrameDfh hdr then 0x8000 else 0x0000
        !sync  = if _tmFrameSync hdr then 0x4000 else 0x0000
        !order = if _tmFrameOrder hdr then 0x2000 else 0x0000
        !seg   = case _tmFrameSegID hdr of
            TMSegment256   -> 0x0000
            TMSegment512   -> 0x0800
            TMSegment1024  -> 0x1000
            TMSegment65536 -> 0x1800
        !fhp    = _tmFrameFirstHeaderPtr hdr .&. 0x7FF
        !result = dfh .|. sync .|. order .|. seg .|. fhp
    in  result

{-# INLINABLE unpackDFS #-}
unpackDFS :: Word16 -> (Bool, Bool, Bool, TMSegmentLen, Word16)
unpackDFS i = (dfh, sync, order, seg, fhp)
  where
    !dfh   = (i .&. 0x8000) /= 0
    !sync  = (i .&. 0x4000) /= 0
    !order = (i .&. 0x2000) /= 0
    !seg   = case i .&. 0x1800 of
        0x0000 -> TMSegment256
        0x0800 -> TMSegment512
        0x1000 -> TMSegment1024
        0x1800 -> TMSegment65536
        _      -> TMSegment1024
    !fhp = i .&. 0x7FF


{-# INLINABLE decodeFrame #-}
decodeFrame :: TMFrameConfig -> ByteString -> Either String TMFrame
decodeFrame cfg = A.parseOnly (tmFrameParser cfg)


{-# INLINABLE encodeFrame #-}
encodeFrame :: TMFrameConfig -> TMFrame -> ByteString
encodeFrame cfg frame =
    let encFrame' = builderBytes (tmFrameBuilder frame)
        !encFrame = tmFrameAppendCRC cfg encFrame'
    in  encFrame


{-# INLINABLE tmFrameCheckCRC #-}
tmFrameCheckCRC :: TMFrameConfig -> ByteString -> Either Text ()
tmFrameCheckCRC cfg bs = if cfgTMFrameHasCRC cfg
    then case crcCheck bs of
        Left  err -> Left err
        Right _   -> Right ()
    else Right ()




{-# INLINABLE tmFrameHeaderBuilder #-}
tmFrameHeaderBuilder :: TMFrameHeader -> Builder
tmFrameHeaderBuilder hdr =
    word16BE (packFlags hdr)
        <> word8 (_tmFrameMCFC hdr)
        <> word8 (_tmFrameVCFC hdr)
        <> word16BE (packDFS hdr)

{-# INLINABLE tmFrameHeaderParser #-}
tmFrameHeaderParser :: Parser TMFrameHeader
tmFrameHeaderParser = do
    (vers, scid, vcid, opc)      <- unpackFlags <$> A.anyWord16be
    mc                           <- A.anyWord8
    vc                           <- A.anyWord8
    (dfh, sync, order, seg, fhp) <- unpackDFS <$> A.anyWord16be
    return (TMFrameHeader vers scid vcid opc mc vc dfh sync order seg fhp)


-- | TODO: currently, only GAIA secondary header is handled !!!!
{-# INLINABLE tmFrameParser #-}
tmFrameParser :: TMFrameConfig -> Parser TMFrame
tmFrameParser cfg = do
    hdr    <- tmFrameHeaderParser

    secHdr <- if _tmFrameDfh hdr
        then do
            _  <- A.anyWord8
            b1 <- A.anyWord8
            b2 <- A.anyWord8
            b3 <- A.anyWord8
            let !val =
                    (fromIntegral b1 `shiftL` 24)
                        .|. (fromIntegral b2 `shiftL` 16)
                        .|. (fromIntegral b3 `shiftL` 8)
                        .|. fromIntegral (_tmFrameVCFC hdr)
            return (TMFrameGAIASecHeader val)
        else return TMFrameEmptySecHeader

    let stdLen = tmFrameMaxPayloadLen cfg hdr
        !payloadLen =
            stdLen
                - (tmSecHdrLen secHdr)
                - (if _tmFrameOpControl hdr then fixedSizeOf @CLCW else 0)
                - (if cfgTMFrameHasCRC cfg then fixedSizeOf @CRC else 0)

    payload <- A.take payloadLen

    clcw    <- if _tmFrameOpControl hdr
        then Just <$> A.anyWord32be
        else return Nothing

    crc <- if cfgTMFrameHasCRC cfg then Just <$> crcParser else return Nothing

    return $! TMFrame hdr secHdr payload clcw crc


{-# INLINABLE tmFrameGetPrevAndRest #-}
tmFrameGetPrevAndRest :: TMFrame -> (ByteString, ByteString)
tmFrameGetPrevAndRest frame =
    let hdrPtr = frame ^. tmFrameHdr . tmFrameFirstHeaderPtr
        dat    = frame ^. tmFrameData
        prev   = if
            | hdrPtr == tmFrameNoFirstHeader -> dat
            | hdrPtr == tmFrameIdlePtr       -> B.empty
            | otherwise                      -> B.take (fromIntegral hdrPtr) dat
        rest = if
            | hdrPtr == tmFrameNoFirstHeader -> B.empty
            | hdrPtr == tmFrameIdlePtr       -> B.empty
            | otherwise                      -> B.drop (fromIntegral hdrPtr) dat
    in  (prev, rest)


{-# INLINABLE tmFrameAppendCRC #-}
tmFrameAppendCRC :: TMFrameConfig -> ByteString -> ByteString
tmFrameAppendCRC config encFrame =
    if cfgTMFrameHasCRC config then crcEncodeAndAppendBS encFrame else encFrame

