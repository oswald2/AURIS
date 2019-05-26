{-# LANGUAGE OverloadedStrings
    , BangPatterns
    , NoImplicitPrelude
    , DeriveGeneric
    , TypeApplications
    , MultiWayIf
    , TemplateHaskell
#-}
module Data.PUS.TMFrame
    ( TMSegmentLen(..)
    , TMFrameHeader
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
    , tmFrameData
    , tmFrameOCF 
    , tmFrameFECW
    , tmFrameBuilder
    , tmFrameParser
    , makeTMFrame
    , tmFrameMaxDataLen
    , tmFrameMinLen
    , tmFrameGetPrevAndRest
    , decodeFrame
    , tmFrameAppendCRC
    , tmFrameEncodeC
    , tmFrameDecodeC
    )
where


import           RIO                     hiding ( Builder )
import qualified RIO.ByteString                as B
import qualified RIO.Text as T

import           Control.Lens                   ( makeLenses )
import Conduit
import Control.PUS.Classes

import           Data.Attoparsec.ByteString     ( Parser )
import qualified Data.Attoparsec.ByteString    as A
import qualified Data.Attoparsec.Binary        as A
import           Data.Bits
import           Data.Conduit.Attoparsec

import           ByteString.StrictBuilder

import           Data.PUS.CLCW
import           Data.PUS.CRC
import           Data.PUS.Types
import           Data.PUS.Config
import           Data.PUS.MissionSpecific.Definitions
import           Data.PUS.TMFrameDfh
import           Data.PUS.Events

import           Protocol.SizeOf


data TMSegmentLen = TMSegment256
    | TMSegment512
    | TMSegment1024
    | TMSegment65536
      deriving (Show, Read, Eq, Ord, Enum, Generic)

data TMFrameHeader = TMFrameHeader {
    _tmFrameVersion :: !Word8,
    _tmFrameScID :: !SCID,
    _tmFrameVcID :: !VCID,
    _tmFrameOpControl :: !Bool,
    _tmFrameMCFC :: !Word8,
    _tmFrameVCFC :: !Word8,
    _tmFrameDfh :: !Bool,
    _tmFrameSync :: !Bool,
    _tmFrameOrder :: !Bool,
    _tmFrameSegID :: !TMSegmentLen,
    _tmFrameFirstHeaderPtr :: !Word16
} deriving (Show, Read, Generic)
makeLenses ''TMFrameHeader

instance SizeOf TMFrameHeader where
    sizeof _ = 6

instance FixedSize TMFrameHeader where
    fixedSizeOf = 6


tmFrameIdlePtr :: Word16
tmFrameIdlePtr = 0x7FE

tmFrameNoFirstHeader :: Word16
tmFrameNoFirstHeader = 0x7FF

tmSegmentLength :: TMSegmentLen -> Int
tmSegmentLength TMSegment256   = 256
tmSegmentLength TMSegment512   = 512
tmSegmentLength TMSegment1024  = 1024
tmSegmentLength TMSegment65536 = 65536


data TMFrame = TMFrame {
    _tmFrameHdr :: TMFrameHeader,
    _tmFrameData :: !ByteString,
    _tmFrameOCF :: Maybe CLCW,
    _tmFrameFECW :: Maybe CRC
} deriving (Show, Read, Generic)
makeLenses ''TMFrame


tmFrameBuilder :: TMFrame -> Builder
tmFrameBuilder f =
    tmFrameHeaderBuilder (_tmFrameHdr f)
        <> bytes (_tmFrameData f)
        <> case (_tmFrameOCF f) of
               Just c  -> clcwBuilder c
               Nothing -> mempty



{-# INLINABLE makeTMFrame #-}
makeTMFrame :: TMFrameHeader -> ByteString -> TMFrame
makeTMFrame hdr pl = TMFrame hdr pl Nothing Nothing

{-# INLINABLE tmFrameMaxDataLen #-}
tmFrameMaxDataLen :: Config -> PUSMissionSpecific -> TMFrameHeader -> Int
tmFrameMaxDataLen cfg missionSpecific hdr =
    fromIntegral (cfgMaxTMFrameLen cfg)
        - fixedSizeOf @TMFrameHeader
        - dfhLen
        - opLen
        - fecLen
  where
    opLen  = if _tmFrameOpControl hdr then fixedSizeOf @CLCW else 0
    fecLen = if (cfgTMFrameHasCRC cfg) then fixedSizeOf @CRC else 0
    dfhLen = case missionSpecific ^. pmsTMFrameDataFieldHeader of
        Just h -> tmFrameDfhLength h
        Nothing  -> 0


{-# INLINABLE tmFrameMaxPayloadLen #-}
tmFrameMaxPayloadLen :: Config -> TMFrameHeader -> Int
tmFrameMaxPayloadLen conf _ = fromIntegral (cfgMaxTMFrameLen conf)
    - fixedSizeOf @TMFrameHeader


{-# INLINABLE tmFrameMinLen #-}
tmFrameMinLen :: Int
tmFrameMinLen = fixedSizeOf @TMFrameHeader + 1


{-# INLINABLE packFlags #-}
packFlags :: TMFrameHeader -> Word16
packFlags hdr =
    let
        !vers = (fromIntegral (_tmFrameVersion hdr)) .&. 0x03
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
        !fhp    = (_tmFrameFirstHeaderPtr hdr) .&. 0x7FF
        !result = dfh .|. sync .|. order .|. seg .|. fhp
    in  result

{-# INLINABLE unpackDFS #-}
unpackDFS :: Word16 -> (Bool, Bool, Bool, TMSegmentLen, Word16)
unpackDFS i = (dfh, sync, order, seg, fhp)
  where
    !dfh   = (i .&. 0x8000) /= 0
    !sync  = (i .&. 0x4000) /= 0
    !order = (i .&. 0x2000) /= 0
    !seg   = case (i .&. 0x1800) of
        0x0000 -> TMSegment256
        0x0800 -> TMSegment512
        0x1000 -> TMSegment1024
        0x1800 -> TMSegment65536
        _      -> TMSegment1024
    !fhp = i .&. 0x7FF


decodeFrame :: Config -> ByteString -> Either String TMFrame
decodeFrame cfg input = A.parseOnly (tmFrameParser cfg) input


tmFrameEncodeC :: (MonadReader env m, HasConfig env) => ConduitT TMFrame ByteString m ()
tmFrameEncodeC = awaitForever $ \frame -> do
    cfg <- view getConfig
    let enc = builderBytes (tmFrameBuilder frame)
        result = tmFrameAppendCRC cfg enc
    yield result


tmFrameDecodeC :: (MonadIO m, MonadReader env m, HasGlobalState env) => ConduitT ByteString TMFrame m ()
tmFrameDecodeC = do
    cfg <- view getConfig
    conduitParserEither (A.match (tmFrameParser cfg)) .| proc cfg
    where
        proc cfg = awaitForever $ \x -> do
            st <- ask
            case x of
                Left err -> do
                    let msg = T.pack (errorMessage err)
                    liftIO $ raiseEvent st (EVAlarms (EV_IllegalTMFrame msg))
                    proc cfg
                Right (_, (bs, frame)) -> do
                    case checkFrame cfg bs of
                        Left err -> do
                            liftIO $ raiseEvent st (EVAlarms (EV_IllegalTMFrame err))
                            proc cfg
                        Right () -> do
                            yield frame
                            proc cfg

checkFrame :: Config -> ByteString -> Either Text ()
checkFrame cfg bs = 
    if cfgTMFrameHasCRC cfg 
        then 
            case crcCheck bs of 
                Left err -> Left err
                Right _ -> Right () 
        else Right ()
    



tmFrameHeaderBuilder :: TMFrameHeader -> Builder
tmFrameHeaderBuilder hdr =
    word16BE (packFlags hdr)
        <> word8 (_tmFrameMCFC hdr)
        <> word8 (_tmFrameVCFC hdr)
        <> word16BE (packDFS hdr)

tmFrameHeaderParser :: Parser TMFrameHeader
tmFrameHeaderParser = do
    (vers, scid, vcid, opc)      <- unpackFlags <$> A.anyWord16be
    mc                           <- A.anyWord8
    vc                           <- A.anyWord8
    (dfh, sync, order, seg, fhp) <- unpackDFS <$> A.anyWord16be
    return (TMFrameHeader vers scid vcid opc mc vc dfh sync order seg fhp)

tmFrameParser :: Config -> Parser TMFrame
tmFrameParser cfg = do
    hdr <- tmFrameHeaderParser
    let stdLen = tmFrameMaxPayloadLen cfg hdr
        payloadLen =
            stdLen
                - (if _tmFrameOpControl hdr
                      then fixedSizeOf @CLCW
                      else 0
                  )
                - (if cfgTMFrameHasCRC cfg
                      then fixedSizeOf @CRC
                      else 0
                  )
    payload <- A.take payloadLen

    clcw <- if _tmFrameOpControl hdr then Just <$> clcwParser else return Nothing

    crc <- if cfgTMFrameHasCRC cfg then Just <$> crcParser else return Nothing

    return $! TMFrame hdr payload clcw crc


{-# INLINABLE tmFrameGetPrevAndRest #-}
tmFrameGetPrevAndRest :: TMFrame -> (ByteString, ByteString)
tmFrameGetPrevAndRest frame =
    let hdrPtr = frame ^. tmFrameHdr . tmFrameFirstHeaderPtr
        dat    = frame ^. tmFrameData
        prev   = if
            | hdrPtr == tmFrameNoFirstHeader -> B.empty
            | hdrPtr == tmFrameIdlePtr       -> B.empty
            | otherwise                      -> B.take (fromIntegral hdrPtr) dat
        rest = B.drop (fromIntegral hdrPtr) dat
    in  (prev, rest)


{-# INLINABLE tmFrameAppendCRC #-}
tmFrameAppendCRC :: Config -> ByteString -> ByteString
tmFrameAppendCRC config encFrame = if (cfgTMFrameHasCRC config)
    then crcEncodeAndAppendBS encFrame
    else encFrame



-- {-# INLINABLE calcHdrLengths #-}
-- calcHdrLengths :: Config -> Int -> Int
-- calcHdrLengths config len =
--     if (cfgTMFrameHasCRC config) then len + fromIntegral crcLen else len
