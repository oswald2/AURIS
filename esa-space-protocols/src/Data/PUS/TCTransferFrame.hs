{-# LANGUAGE OverloadedStrings
    , NoImplicitPrelude
    , TemplateHaskell
    , BangPatterns
#-}
module Data.PUS.TCTransferFrame
    (
      -- | A conduit to encode TC Frames
      tcFrameHeaderLen
    , tcFrameEncode
    , tcFrameEncodeC
    , tcFrameDecodeC
    , checkTCFrame
    , tcFrameParser
    , tcFrameBuilder
    , tcFrameEncodeSimple
    , tcFrameEncodeSimpleWoCRC
    ) where


import           RIO                     hiding ( (.~)
                                                , Builder
                                                )
import qualified RIO.ByteString                as BS
import qualified RIO.Text                      as T

import           Control.Lens                   ( (.~) )
import           Control.PUS.Classes            ( HasConfig(getConfig)
                                                , HasGlobalState
                                                , HasPUSState(appStateG)
                                                , raiseEvent
                                                )

import           ByteString.StrictBuilder       ( Builder
                                                , builderBytes
                                                , bytes
                                                , word16BE
                                                , word8
                                                )
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
import           Data.Conduit                   ( (.|)
                                                , ConduitT
                                                , await
                                                , awaitForever
                                                , yield
                                                )
import           Data.Conduit.Attoparsec        ( ParseError(errorMessage)
                                                , conduitParserEither
                                                )

import           Data.PUS.CRC                   ( crcCalc
                                                , crcEncodeBS
                                                , crcLen
                                                , crcParser
                                                )
import           Data.PUS.Config                ( Config(cfgSCID, cfgVCIDs) )
import           Data.PUS.Events                ( Event(EVAlarms)
                                                , EventAlarm(EVIllegalTCFrame)
                                                )
import           Data.PUS.GlobalState           ( nextADCount )
import           Data.PUS.TCFrameTypes

import           General.PUSTypes               ( SCID
                                                , getSCID
                                                , getVCID
                                                , mkSCID
                                                , mkVCID
                                                )
import           General.Types                  ( HexBytes(..)
                                                , toBS
                                                )


-- | The lenght of the TC Transfer Frame Header in Bytes
tcFrameHeaderLen :: Int
tcFrameHeaderLen = 5



checkTCFrame :: Config -> TCTransferFrame -> Either Text ()
checkTCFrame cfg frame =
    let test =
            [ frame ^. tcFrameVersion == 0
            , frame ^. tcFrameSCID == cfgSCID cfg
            , frame ^. tcFrameVCID `elem` cfgVCIDs cfg
            , case flag of
                FrameAD      -> True
                FrameBD      -> seqc == 0
                FrameBC      -> seqc == 0
                FrameIllegal -> False
            ]
        flag = frame ^. tcFrameFlag
        seqc = frame ^. tcFrameSeq
    in  if all (== True) test
            then Right ()
            else
                Left
                $ "TC Frame Header Error [Version, S/C ID, VC ID, Frame Type]: "
                <> T.pack (show test)




{-# INLINABLE tcFrameEncode #-}
tcFrameEncode :: TCFrameTransport -> Word8 -> EncodedTCFrame
tcFrameEncode (TCFrameTransport frame rqst) frameCnt =
    let newFrame = tcFrameEncodeSimple frame frameCnt
    in  EncodedTCFrame (frame ^. tcFrameSeq) (HexBytes newFrame) rqst


{-# INLINABLE tcFrameEncodeSimple #-}
tcFrameEncodeSimple :: TCTransferFrame -> Word8 -> ByteString
tcFrameEncodeSimple frame frameCnt =
    let newFrame = tcFrameEncodeSimpleWoCRC frame frameCnt 
    in  newFrame <> crcEncodeBS (crcCalc newFrame)

{-# INLINABLE tcFrameEncodeSimpleWoCRC #-}
tcFrameEncodeSimpleWoCRC :: TCTransferFrame -> Word8 -> ByteString 
tcFrameEncodeSimpleWoCRC frame frameCnt = 
    let newFrame =
            frame
                &  tcFrameLength
                .~
                    -- frame length is complete frame (header + data + CRC - 1)
                   fromIntegral
                       (tcFrameHeaderLen + BS.length newPl + crcLen - 1)
                &  tcFrameSeq
                .~ frameCnt
        pl       = toBS $ frame ^. tcFrameData
        newPl    = if BS.null pl then BS.singleton 0 else pl
        encFrame = builderBytes $ tcFrameBuilder newFrame
    in  encFrame 

{-# INLINABLE tcFrameBuilder #-}
tcFrameBuilder :: TCTransferFrame -> Builder
tcFrameBuilder frame =
    word16BE (packFlags frame)
        <> word16BE (packLen frame)
        <> word8 (frame ^. tcFrameSeq)
        <> bytes (toBS (frame ^. tcFrameData))


tcFrameParser :: Parser TCTransferFrame
tcFrameParser = do
    (dat, (frame, crc)) <- A.match tcFrameParser'
    -- now also check the CRC
    let enc           = BS.take (BS.length dat - crcLen) dat
        calculatedCRC = crcCalc enc
    if crc /= calculatedCRC
        then fail
            (  "TC Transfer Frame: CRC does not match: received: "
            <> show crc
            <> ", calculated: "
            <> show calculatedCRC
            )
        else pure frame
  where
    tcFrameParser' = do
        flags <- A.anyWord16be
        word2 <- A.anyWord16be
        seqf  <- A.anyWord8

        let (vers, fl, scid) = unpackFlags flags
            vcid             = mkVCID (fromIntegral (word2 `shiftR` 10))
            !length1         = word2 .&. 0b0000_0011_1111_1111
            !length2         = length1 + 1
            newLen           = fromIntegral length2 - tcFrameHdrLen - crcLen

        when (newLen < 0)
            $  fail
            $  "Illegal Length in TC Frame Header: "
            ++ show length1

        pl  <- A.take newLen
        crc <- crcParser

        pure (TCTransferFrame vers fl scid vcid length1 seqf (HexBytes pl), crc)




-- | A conduit for encoding a TC Transfer Frame into a ByteString for transmission
{-# INLINABLE tcFrameEncodeC #-}
tcFrameEncodeC
    :: (MonadIO m, MonadReader env m, HasGlobalState env)
    => ConduitT TCFrameTransport EncodedTCFrame m ()
tcFrameEncodeC = do
    f <- await
    case f of
        Just frame -> do
            logDebug $ "Got Frame: " <> displayShow (frame ^. tcfTransFrame)
            case frame ^. tcfTransFrame . tcFrameFlag of
                FrameAD -> do
                    st  <- view appStateG
                    cnt <- liftIO . atomically $ nextADCount st
                    yield (tcFrameEncode frame cnt)
                    tcFrameEncodeC
                FrameBD -> do
                    yield $ tcFrameEncode frame 0
                    tcFrameEncodeC
                FrameBC -> do
                    st  <- view appStateG
                    cnt <- liftIO . atomically $ nextADCount st
                    yield $ tcFrameEncode frame cnt
                    tcFrameEncodeC
                FrameIllegal -> do
                    raiseEvent
                        $ EVAlarms
                              (EVIllegalTCFrame
                                  "Illegal Frame on encode, frame discarded"
                              )
                    tcFrameEncodeC
        Nothing -> pure ()



tcFrameDecodeC
    :: (MonadIO m, MonadReader env m, HasGlobalState env)
    => ConduitT ByteString TCTransferFrame m ()
tcFrameDecodeC = conduitParserEither tcFrameParser .| proc
  where
    proc = awaitForever $ \x -> do
        case x of
            Left err -> do
                let msg = T.pack (errorMessage err)
                raiseEvent (EVAlarms (EVIllegalTCFrame msg))
                proc
            Right (_, frame) -> do
                cfg <- view getConfig
                case checkTCFrame cfg frame of
                    Left err -> do
                        raiseEvent (EVAlarms (EVIllegalTCFrame err))
                        proc
                    Right () -> do
                        yield frame
                        proc


{-# INLINABLE packFlags #-}
packFlags :: TCTransferFrame -> Word16
packFlags frame =
    let !vers   = fromIntegral (frame ^. tcFrameVersion)
        !flag   = frame ^. tcFrameFlag
        !scid   = getSCID (frame ^. tcFrameSCID) .&. 0x03FF
        encFlag = case flag of
            FrameAD      -> 0
            FrameBD      -> 0x2000
            FrameBC      -> 0x3000
            FrameIllegal -> 0x2000
        !flags = (vers `shiftL` 14) .|. encFlag .|. scid
    in  flags


{-# INLINABLE unpackFlags #-}
unpackFlags :: Word16 -> (Word8, TCFrameFlag, SCID)
unpackFlags i = (vers, fl, scid)
  where
    !vers = fromIntegral (((i .&. 0x3FFF) `shiftR` 14) .&. 0x0003)
    !scid = mkSCID $ i .&. 0x03FF
    !fl   = case i .&. 0x3000 of
        0      -> FrameAD
        0x2000 -> FrameBD
        0x3000 -> FrameBC
        _      -> FrameIllegal


{-# INLINABLE packLen #-}
packLen :: TCTransferFrame -> Word16
packLen frame =
    let !len = frame ^. tcFrameLength
        vcid :: Word16
        !vcid   = fromIntegral $ getVCID (frame ^. tcFrameVCID)
        !result = vcid `shiftL` 10 .|. (len .&. 0x3FF)
    in  result



