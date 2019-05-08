{-# LANGUAGE OverloadedStrings
    , NoImplicitPrelude
    , TemplateHaskell
    , BangPatterns
#-}
module Data.PUS.TCTransferFrame
    (
      -- | The TC Transfer Frame itself
      TCTransferFrame(..)
      -- | Header Flags
    , TCFrameFlag(..)
      -- | The TC directives
    , TCDirective(..)
    , EncodedTCFrame
    , tcFrameHeaderLen
    , tcFrameEncode
    , tcFrameVersion
    , tcFrameFlag
    , tcFrameSCID
    , tcFrameVCID
    , tcFrameLength
    , tcFrameSeq
      -- | A conduit to encode TC Frames
    , tcFrameEncodeC
    , tcFrameDecodeC
    , encTcFrameSeq
    , encTcFrameData
    , directiveBuilder
    , directiveParser
    , checkTCFrame
    , tcFrameParser

    )
where


import           RIO
import qualified RIO.ByteString                as BS
import qualified RIO.ByteString.Lazy           as BL
import qualified RIO.Text                      as T

import           Control.Lens                   ( makeLenses )
import           Control.PUS.Monads

import           Data.Bits
import           Data.Conduit
import           Data.ByteString.Builder
import           Data.Attoparsec.ByteString     ( Parser )
import qualified Data.Attoparsec.ByteString    as A
import qualified Data.Attoparsec.Binary        as A
import           Data.Conduit.Attoparsec

import           Data.PUS.CRC
import           Data.PUS.Config
import           Data.PUS.Types
import           Data.PUS.Events

-- | indicates, which type this TC Frame is. AD/BD specifies the protocol mode
-- (AD = sequence controlled, BD = expedited), BC is a directive (see 'TCDirective')
data TCFrameFlag =
    FrameAD
    | FrameBD
    | FrameBC
    | FrameIllegal
    deriving (Eq, Ord, Enum, Show, Read)


-- | A TC Transfer Frame 
data TCTransferFrame = TCTransferFrame {
    _tcFrameVersion :: !Word8
    , _tcFrameFlag :: !TCFrameFlag
    , _tcFrameSCID :: !SCID
    , _tcFrameVCID :: !VCID
    , _tcFrameLength :: !Word16
    , _tcFrameSeq :: !Word8
    , _tcFrameData :: BS.ByteString
    } deriving (Eq, Show, Read)

makeLenses ''TCTransferFrame


data EncodedTCFrame = EncodedTCFrame {
    _encTcFrameSeq :: !Word8
    , _encTcFrameData :: BS.ByteString
    } deriving (Show, Read)

makeLenses ''EncodedTCFrame



-- | A TC directive for the on-board decoder
data TCDirective =
    Unlock
    | SetVR !Word8
    | DNop
    deriving (Eq, Show, Read)

-- | The lenght of the TC Transfer Frame Header in Bytes
tcFrameHeaderLen :: Int
tcFrameHeaderLen = 5



checkTCFrame :: Config -> TCTransferFrame -> Either Text ()
checkTCFrame cfg frame =
    let test =
                [ frame ^. tcFrameVersion == 0
                , frame ^. tcFrameSCID == cfgSCID cfg
                , frame ^. tcFrameVCID `elem` (cfgVCIDs cfg)
                , case flag of
                    FrameAD      -> True
                    FrameBD      -> if seqc == 0 then True else False
                    FrameBC      -> if seqc == 0 then True else False
                    FrameIllegal -> False
                ]
        flag = frame ^. tcFrameFlag
        seqc = frame ^. tcFrameSeq
    in  case all (== True) test of
            True -> Right ()
            False ->
                Left
                    $ "TC Frame Header Error [Version, S/C ID, VC ID, Frame Type]: "
                    <> T.pack (show test)




{-# INLINABLE tcFrameEncode #-}
tcFrameEncode :: TCTransferFrame -> Word8 -> EncodedTCFrame
tcFrameEncode frame frameCnt =
    let newFrame =
                set tcFrameLength
                    (fromIntegral (tcFrameHeaderLen + (BS.length newPl)))
                    $ set tcFrameSeq frameCnt frame
        pl       = frame ^. tcFrameData
        newPl    = if BS.null pl then BS.singleton 0 else pl
        encFrame = toLazyByteString $ tcFrameBuilder newFrame
    in  EncodedTCFrame
            (frame ^. tcFrameSeq)
            (BL.toStrict $ encFrame <> crcEncodeBL (crcCalcBL encFrame))


{-# INLINABLE tcFrameBuilder #-}
tcFrameBuilder :: TCTransferFrame -> Builder
tcFrameBuilder frame =
    word16BE (packFlags frame)
        <> word16BE (packLen frame)
        <> word8 (frame ^. tcFrameSeq)
        <> byteString (frame ^. tcFrameData)


tcFrameParser :: Parser TCTransferFrame
tcFrameParser = do
    flags <- A.anyWord16be
    vc    <- A.anyWord8
    len   <- A.anyWord8
    seqf  <- A.anyWord8

    let (vers, fl, scid) = unpackFlags flags
        vcid             = mkVCID (vc `shiftR` 2)
        len1 :: Word16
        !len1 = (fromIntegral (vc .&. 0x03)) `shiftL` 8
        len2 :: Word16
        !len2    = fromIntegral len
        !length1 = (len1 .|. len2)
        !length2 = length1 + 1

    pl <- A.take (fromIntegral length2)

    pure (TCTransferFrame vers fl scid vcid length1 seqf pl)




-- | A conduit for encoding a TC Transfer Frame into a ByteString for transmission
{-# INLINABLE tcFrameEncodeC #-}
tcFrameEncodeC
    :: (Monad m, MonadPUSState m)
    => ConduitT TCTransferFrame EncodedTCFrame m ()
tcFrameEncodeC = do
    f <- await
    case f of
        Just frame -> do
            case frame ^. tcFrameFlag of
                FrameAD -> do
                    yieldM $ tcFrameEncode frame <$> nextADCount
                    tcFrameEncodeC
                FrameBD -> do
                    yield $ tcFrameEncode frame 0
                    tcFrameEncodeC
                FrameBC -> do
                    yield $ tcFrameEncode frame 0
                    tcFrameEncodeC
                FrameIllegal -> do
                    lift
                        $ raiseEvent
                              (EVIllegalTCFrame
                                  "Illegal Frame on encode, frame discarded"
                              )
                    tcFrameEncodeC
        Nothing -> pure ()



tcFrameDecodeC
    :: (MonadPUSState m, MonadConfig m) => ConduitT ByteString TCTransferFrame m ()
tcFrameDecodeC = conduitParserEither tcFrameParser .| proc
  where
    proc
        :: (MonadPUSState m, MonadConfig m)
        => ConduitT
               (Either ParseError (PositionRange, TCTransferFrame))
               TCTransferFrame
               m
               ()
    proc = awaitForever $ \x -> do
        case x of
            Left err -> do
                let msg = T.pack (errorMessage err)
                lift $ raiseEvent (EVIllegalTCFrame msg)
                proc
            Right (_, frame) -> do
                cfg <- lift $ getConfig
                case checkTCFrame cfg frame of
                    Left err -> do
                        lift $ raiseEvent (EVIllegalTCFrame err)
                        proc
                    Right () -> do
                        yield frame
                        proc


{-# INLINABLE packFlags #-}
packFlags :: TCTransferFrame -> Word16
packFlags frame =
    let !vers   = fromIntegral (frame ^. tcFrameVersion)
        !flag   = frame ^. tcFrameFlag
        !scid   = (getSCID (frame ^. tcFrameSCID)) .&. 0x03FF
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
    !fl   = case (i .&. 0x3000) of
        0      -> FrameAD
        0x2000 -> FrameBD
        0x3000 -> FrameBC
        _      -> FrameIllegal


{-# INLINABLE packLen #-}
packLen :: TCTransferFrame -> Word16
packLen frame =
    let !len  = frame ^. tcFrameLength
        !vcid = getVCID (frame ^. tcFrameVCID)
        !result =
                (fromIntegral vcid)
                    `shiftL` 2
                    .|.      ((len .&. 0x300) `shiftR` 8)
                    .|.      (len .&. 0xFF)
    in  result



{-# INLINABLE directiveBuilder #-}
directiveBuilder :: TCDirective -> Builder
directiveBuilder DNop        = mempty
directiveBuilder Unlock      = word8 0
directiveBuilder (SetVR val) = word8 0x82 <> word8 0 <> word8 val



directiveParser :: Parser TCDirective
directiveParser = do
    b <- A.anyWord8
    case b of
        0    -> return Unlock
        0x82 -> do
            _   <- A.anyWord8
            val <- A.anyWord8
            return (SetVR val)
        _ -> return DNop
