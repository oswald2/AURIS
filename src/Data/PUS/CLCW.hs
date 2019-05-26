{-|
Module      : Data.PUS.CLCW
Description : Data type for the command link control word
Copyright   : (c) Michael Oswald, 2019
License     : BSD-3
Maintainer  : michael.oswald@onikudaki.net
Stability   : experimental
Portability : POSIX

This module is about the CLCW data type. The CLCW is downlinked with
TM transfer frames (it is also called OCF, operational control field)
and is used as a state representation of the spacecraft internal FARM-1
state machine on the receiving side of the COP-1 protocol.
|-}
{-# LANGUAGE OverloadedStrings
    , BangPatterns
    , GeneralizedNewtypeDeriving
    , DeriveGeneric
    , RecordWildCards
    , NoImplicitPrelude
    , TemplateHaskell
#-}
module Data.PUS.CLCW
    ( CLCW(..)
    , createCLCW
    , packValues
    , unpackValues
    , defaultCLCW
    , checkCLCW
    , clcwLen
    , clcwBuilder
    , clcwParser
    , clcwType
    , clcwVersion
    , clcwStatus
    , clcwCopInEffect
    , clcwVcID
    , clcwNoRF
    , clcwNoBitLock
    , clcwLockout
    , clcwWait
    , clcwRetrans
    , clcwBCounter
    , clcwReportType
    , clcwReportVal
    )
where

import           RIO
import qualified RIO.Text                      as T

import           Control.Lens                   ( makeLenses )

import           Data.Bits
--import           Data.ByteString.Lazy.Builder  as B
import           ByteString.StrictBuilder      as B
import           Data.Attoparsec.ByteString     ( Parser )
import qualified Data.Attoparsec.Binary        as A

import           Data.PUS.Types

import           Protocol.SizeOf

import           Formatting

-- | The CLCW data type itself
data CLCW = CLCW {
  _clcwType :: !Bool,
  _clcwVersion :: {-# UNPACK #-} !Word8,
  _clcwStatus :: {-# UNPACK #-} !Word8,
  _clcwCopInEffect :: {-# UNPACK #-} !Word8,
  _clcwVcID :: !VCID,
  _clcwNoRF :: !Bool,
  _clcwNoBitLock :: !Bool,
  _clcwLockout :: !Bool,
  _clcwWait :: !Bool,
  _clcwRetrans :: !Bool,
  _clcwBCounter :: {-# UNPACK #-} !Word8,
  _clcwReportType :: !Bool,
  _clcwReportVal :: {-# UNPACK #-} !Word8
  } deriving (Read, Show)
makeLenses ''CLCW

instance Display CLCW where
    textDisplay x =
        T.pack (show x) <> sformat (left 8 '0' %. hex % ": ") (packValues x)



-- | Performs a validity check of the CLCW itself
{-# INLINABLE checkCLCW #-}
checkCLCW :: CLCW -> Either Text Bool
checkCLCW v =
    let test =
                [ v ^. clcwVersion == 0
                , v ^. clcwStatus == 0
                , v ^. clcwCopInEffect == 1
                , v ^. clcwBCounter < 4
                ]
        msg =
                [ "CLCW Version not 0"
                , "CLCW Status not 0"
                , "CLCW COP in Effect not 1"
                , "CLCW B Counter not < 4"
                ]
    in  case all (== True) test of
            True  -> Right True
            False -> Left $ T.unlines [ err | (True, err) <- zip test msg ]

-- | smart constructor for the CLCW
{-# INLINABLE createCLCW #-}
createCLCW
    :: VCID -> Word8 -> Bool -> Bool -> Bool -> Bool -> Bool -> Word8 -> CLCW
createCLCW vcid vr noRF noBitlock lock waitf retrans bcount =
    CLCW False 0 0 1 vcid noRF noBitlock lock waitf retrans bcount False vr

-- | a default value for the CLCW
{-# INLINABLE defaultCLCW #-}
defaultCLCW :: CLCW
defaultCLCW = createCLCW 0 0 False False False False False 0

-- | Specifies the length of an encoded CLCW value in Bytes
clcwLen :: Int
clcwLen = 4

instance FixedSize CLCW where
    fixedSizeOf = 4

-- | Performs the bit fiddling to get the resulting Word32 value from
-- a CLCW. Used in encoding the CLCW for transmission
{-# INLINABLE packValues #-}
packValues :: CLCW -> Word32
packValues v =
    let !t         = if _clcwType v then 0x80000000 else 0
        !vers      = fromIntegral ((_clcwVersion v) .&. 0x03) `shiftL` 29
        !stat      = fromIntegral ((_clcwStatus v) .&. 0x07) `shiftL` 26
        !cie       = fromIntegral ((_clcwCopInEffect v) .&. 0x03) `shiftL` 24
        !vcid      = fromIntegral (getVCID (_clcwVcID v) .&. 0x3f) `shiftL` 18
        !norf      = if _clcwNoRF v then 0x00008000 else 0
        !nobitlock = if _clcwNoBitLock v then 0x00004000 else 0
        !lockout   = if _clcwLockout v then 0x00002000 else 0
        !waitf     = if _clcwWait v then 0x00001000 else 0
        !retrans   = if _clcwRetrans v then 0x00000800 else 0
        !bc        = fromIntegral ((_clcwBCounter v) .&. 0x03) `shiftL` 9
        !rept      = if _clcwReportType v then 0x00000100 else 0
        !val       = fromIntegral $ _clcwReportVal v
        !result =
                t
                    .|. vers
                    .|. stat
                    .|. cie
                    .|. vcid
                    .|. norf
                    .|. nobitlock
                    .|. lockout
                    .|. waitf
                    .|. retrans
                    .|. bc
                    .|. rept
                    .|. val
    in  result

-- | The opposite of 'packValues'. Creates a CLCW from a Word32
{-# INLINABLE unpackValues #-}
unpackValues :: Word32 -> CLCW
unpackValues v =
    let t         = (v .&. 0x80000000) /= 0
        vers      = fromIntegral $ (v .&. 0x60000000) `shiftR` 29
        stat      = fromIntegral $ (v .&. 0x1c000000) `shiftR` 26
        cie       = fromIntegral $ (v .&. 0x03000000) `shiftR` 24
        vcid      = fromIntegral $ (v .&. 0x00fc0000) `shiftR` 18
        norf      = (v .&. 0x00008000) /= 0
        nobitlock = (v .&. 0x00004000) /= 0
        lockout   = (v .&. 0x00002000) /= 0
        waitf     = (v .&. 0x00001000) /= 0
        retrans   = (v .&. 0x00000800) /= 0
        bc        = fromIntegral $ (v .&. 0x00000600) `shiftR` 9
        rept      = (v .&. 0x00000100) /= 0
        val       = fromIntegral $ v .&. 0xFF
    in  CLCW t
             vers
             stat
             cie
             vcid
             norf
             nobitlock
             lockout
             waitf
             retrans
             bc
             rept
             val


-- | A builder for the CLCW
clcwBuilder :: CLCW -> B.Builder
clcwBuilder clcw = word32BE $ packValues clcw

-- | A attoparsec parser for the CLCW
clcwParser :: Parser CLCW
clcwParser = do
    val <- A.anyWord32be
    return (unpackValues val)

