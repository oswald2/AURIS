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
    , encTcFrameSeq
    , encTcFrameData
    )
where


import           RIO
import qualified RIO.ByteString                as BS
import qualified RIO.ByteString.Lazy           as BL

import           Control.Lens                   ( makeLenses )
import           Control.PUS.MonadPUSState

--import           Data.Word
import           Data.Bits
import           Data.Conduit
import           Data.ByteString.Builder

import           Data.PUS.CRC


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
    , _tcFrameSCID :: !Word16
    , _tcFrameVCID :: !Word8
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



-- | A conduit for encoding a TC Transfer Frame into a ByteString for transmission
{-# INLINABLE tcFrameEncodeC #-}
tcFrameEncodeC
    :: (Monad m, MonadPUSState m) => ConduitT TCTransferFrame EncodedTCFrame m ()
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
                FrameIllegal -> tcFrameEncodeC
        Nothing -> pure ()


{-# INLINABLE packFlags #-}
packFlags :: TCTransferFrame -> Word16
packFlags frame =
    let !vers   = fromIntegral (frame ^. tcFrameVersion)
        !flag   = frame ^. tcFrameFlag
        !scid   = (frame ^. tcFrameSCID) .&. 0x03FF
        encFlag = case flag of
            FrameAD      -> 0
            FrameBD      -> 0x2000
            FrameBC      -> 0x3000
            FrameIllegal -> 0x2000
        !flags = (vers `shiftL` 14) .|. encFlag .|. scid
    in  flags

{-# INLINABLE packLen #-}
packLen :: TCTransferFrame -> Word16
packLen frame =
    let !len  = frame ^. tcFrameLength
        !vcid = frame ^. tcFrameVCID
        !result =
                (fromIntegral vcid)
                    `shiftL` 2
                    .|.      ((len .&. 0x300) `shiftR` 8)
                    .|.      (len .&. 0xFF)
    in  result

