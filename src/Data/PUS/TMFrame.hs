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
    (
    TMFrameHeader
    , TMFrame
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
    , tmFrameCheckOrder
    , tmFrameCheckSync
    , isIdleTmFrame
    , tmFrameFHType
    , tmFrameDefaultHeader
    )
where


import           RIO                     hiding ( Builder )
import qualified RIO.ByteString                as B
import qualified RIO.Text                      as T

import           Control.Lens                   ( makeLenses )
import           Conduit
import           Control.PUS.Classes

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



data FirstHeaderPtrVal =
    FirstHeaderZero
    | FirstHeaderNoFH
    | FirstHeaderNonZero
    | FirstHeaderIdle
    deriving (Eq, Ord, Show, Read)


-- | The primary frame header, adheres to the PUS Standard
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

tmFrameDefaultHeader :: TMFrameHeader
tmFrameDefaultHeader = TMFrameHeader {
    _tmFrameVersion = 0,
    _tmFrameScID = mkSCID 0,
    _tmFrameVcID = mkVCID 0,
    _tmFrameOpControl = True,
    _tmFrameMCFC = 0,
    _tmFrameVCFC = 0,
    _tmFrameDfh = False,
    _tmFrameSync = True,
    _tmFrameOrder = True,
    _tmFrameSegID = TMSegment65536,
    _tmFrameFirstHeaderPtr = 0
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
data TMFrame = TMFrame {
    _tmFrameHdr :: TMFrameHeader,
    _tmFrameData :: !ByteString,
    _tmFrameOCF :: Maybe CLCW,
    _tmFrameFECW :: Maybe CRC
} deriving (Show, Read, Generic)
makeLenses ''TMFrame




{-# INLINABLE tmFrameFHType #-}
tmFrameFHType :: TMFrame -> FirstHeaderPtrVal
tmFrameFHType frame =
    let fh = frame ^. tmFrameHdr . tmFrameFirstHeaderPtr
    in
    if | fh == 0 -> FirstHeaderZero
       | fh == tmFrameNoFirstHeader -> FirstHeaderNoFH
       | fh == tmFrameIdlePtr -> FirstHeaderIdle
       | otherwise -> FirstHeaderNonZero



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

tmFrameBuilder :: TMFrame -> Builder
tmFrameBuilder f =
    tmFrameHeaderBuilder (_tmFrameHdr f)
        <> bytes (_tmFrameData f)
        <> case _tmFrameOCF f of
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
    fecLen = if cfgTMFrameHasCRC cfg then fixedSizeOf @CRC else 0
    dfhLen = case missionSpecific ^. pmsTMFrameDataFieldHeader of
        Just h  -> tmFrameDfhLength h
        Nothing -> 0


{-# INLINABLE tmFrameMaxPayloadLen #-}
tmFrameMaxPayloadLen :: Config -> TMFrameHeader -> Int
tmFrameMaxPayloadLen conf _ =
    fromIntegral (cfgMaxTMFrameLen conf) - fixedSizeOf @TMFrameHeader


{-# INLINABLE tmFrameMinLen #-}
tmFrameMinLen :: Int
tmFrameMinLen = fixedSizeOf @TMFrameHeader + 1


{-# INLINABLE packFlags #-}
packFlags :: TMFrameHeader -> Word16
packFlags hdr
    = let
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


decodeFrame :: Config -> ByteString -> Either String TMFrame
decodeFrame cfg = A.parseOnly (tmFrameParser cfg)

-- | Conduit to encode a 'TMFrame'. Just calls the builder on it and yields the
-- resulting 'ByteString'
tmFrameEncodeC
    :: (MonadReader env m, HasConfig env) => ConduitT TMFrame ByteString m ()
tmFrameEncodeC = awaitForever $ \frame -> do
    cfg <- view getConfig
    let enc    = builderBytes (tmFrameBuilder frame)
        result = tmFrameAppendCRC cfg enc
    yield result

-- | Conduit to decode a 'TMFrame'. In case the frame cannot be parsed, a
-- 'EV_IllegalTMFrame' event is raised. If the frame could be parsed, first
-- it is checked if it is an idle-frame. Idle-frames are simply discarded.
--
-- In case it is a normal frame, it is CRC-checked. In case the CRC is invalid,
-- a 'EV_IllegalTMFrame' event with an error message is raised.
--
-- If the frame was ok, it is yield'ed to the next conduit.
tmFrameDecodeC
    :: (MonadIO m, MonadReader env m, HasGlobalState env)
    => ConduitT ByteString TMFrame m ()
tmFrameDecodeC = do
    cfg <- view getConfig
    conduitParserEither (A.match (tmFrameParser cfg)) .| proc cfg
  where
    proc cfg = awaitForever $ \x -> do
        st <- ask
        case x of
            Left err -> do
                let msg = T.pack (errorMessage err)
                liftIO $ raiseEvent st (EVAlarms (EVIllegalTMFrame msg))
                proc cfg
            Right (_, (bs, frame)) ->
                -- if we have an idle-frame, just throw it away
                if isIdleTmFrame frame then proc cfg
                else
                    case checkFrame cfg bs of
                        Left err -> do
                            liftIO $ raiseEvent st (EVTelemetry (EVTMFailedCRC err))
                            proc cfg
                        Right () -> do
                            yield frame
                            proc cfg

checkFrame :: Config -> ByteString -> Either Text ()
checkFrame cfg bs = if cfgTMFrameHasCRC cfg
    then case crcCheck bs of
        Left  err -> Left err
        Right _   -> Right ()
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
                - (if _tmFrameOpControl hdr then fixedSizeOf @CLCW else 0)
                - (if cfgTMFrameHasCRC cfg then fixedSizeOf @CRC else 0)
    payload <- A.take payloadLen

    clcw    <- if _tmFrameOpControl hdr
        then Just <$> clcwParser
        else return Nothing

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
        rest =  if
            | hdrPtr == tmFrameNoFirstHeader -> dat
            | hdrPtr == tmFrameIdlePtr       -> B.empty
            | otherwise                      -> B.drop (fromIntegral hdrPtr) dat
    in  (prev, rest)


{-# INLINABLE tmFrameAppendCRC #-}
tmFrameAppendCRC :: Config -> ByteString -> ByteString
tmFrameAppendCRC config encFrame = if cfgTMFrameHasCRC config
    then crcEncodeAndAppendBS encFrame
    else encFrame

