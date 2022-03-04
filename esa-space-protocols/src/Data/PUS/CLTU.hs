{-|
Module      : Data.PUS.CLTU
Description : Provides functions for CLTU (Command Link Transfer Unit) handling
Copyright   : (c) Michael Oswald, 2019
License     : BSD-3
Maintainer  : michael.oswald@onikudaki.net
Stability   : experimental
Portability : POSIX

This module is used for encoding and decoding to and from CLTU's. A TC Randomizer can be used, which is
basically an array of values which are xor'ed with the CLTU block data so that the distribution of
0's and 1's is more even (better for transmission to the satellite).
-}
{-# LANGUAGE BangPatterns
    , OverloadedStrings
    , NoImplicitPrelude
#-}
module Data.PUS.CLTU
    ( CLTU
    , CLTUInput(..)
    , EncodedCLTU(..)
    , cltuNew
    , cltuPayLoad
    , encode
    , encodeRandomized
    , cltuParser
    , cltuRandomizedParser
    , cltuDecodeC
    , cltuDecodeRandomizedC
    , cltuEncodeC
    , cltuEncodeRandomizedC
    , cltuParity
    , showEncodedCLTU
    )
where

import           RIO                     hiding ( Builder )
import qualified RIO.ByteString                as BS

import Control.Monad.State ( State, evalState )
import Control.PUS.Classes ( HasConfig(..) )

import ByteString.StrictBuilder
    ( Builder, builderBytes, bytes, word8 )

import Data.Bits ( Bits((.&.), xor, shiftL) )
import qualified Data.Text                     as T
import qualified Data.Text.Lazy                as TL
import qualified Data.Text.Lazy.Builder        as TB
import           Data.List                     as L
                                                ( intersperse )

import Data.PUS.Config
    ( cltuBlockSizeAsWord8,
      Config(cfgCltuBlockSize, cfgRandomizerStartValue) )
import Data.PUS.CLTUTable ( cltuTable )
import Data.PUS.Randomizer ( initialize, randomize, Randomizer )
import Data.PUS.TCRequest ( TCRequest )

import qualified TextShow                      as TS
import TextShow.Data.Integral ( showbHex )

import General.Chunks ( chunkedByBS )
import General.Hexdump ( hexdumpLineBS )


import           Data.Attoparsec.ByteString     ( Parser )
import qualified Data.Attoparsec.ByteString    as A

import Data.Conduit ( ConduitT, awaitForever, yield )
import Data.Conduit.Attoparsec
    ( conduitParserEither, ParseError, PositionRange )


-- The CLTU itself
newtype CLTU = CLTU {
    -- | returns the actual binary payload data (mostly a TC transfer frame)
    cltuPayLoad :: BS.ByteString
}


data CLTUInput = CLTUInput {
    _cltuInpCLTU :: !CLTU
    , _cltuInpRequest :: !TCRequest 
    }

data EncodedCLTU = EncodedCLTU {
    cltuEncoded :: BS.ByteString
    , cltuRequest :: !TCRequest 
}

-- | The PUS Standard explicitly states, that filling bytes (0x55) may be
-- inserted at the end of a code block to padRight the length. It also states
-- that these padding bytes shall be removed on the next higher layer
--
-- So this makes an own Eq instance necessary, which ignores the fill bytes
instance Eq CLTU where
    CLTU bs1 == CLTU bs2 = all (== True) $ BS.zipWith (==) bs1 bs2


instance Show CLTU where
    show (CLTU x) = "CLTU:\n" <> T.unpack (hexdumpLineBS x)


showEncodedCLTU :: Config -> EncodedCLTU -> Text
showEncodedCLTU cfg (EncodedCLTU bs _) = TL.toStrict . TB.toLazyText $ result
  where
    header  = TB.fromText (hexdumpLineBS (BS.take 2 bs))
    chunksb = chunkedByBS
        (fromIntegral (cltuBlockSizeAsWord8 (cfgCltuBlockSize cfg)))
        (BS.drop 2 bs)
    builders =
        mconcat
            . intersperse (TB.fromText " ")
            . map (TB.fromText . hexdumpLineBS)
            $ chunksb
    result = header <> TB.singleton ' ' <> builders

{-# INLINABLE cltuNew #-}
-- | create a CLTU from a payload, which will mostly be a TC transfer frame
cltuNew :: ByteString -> CLTU
cltuNew = CLTU

{-# INLINABLE cltuHeader #-}
-- | The CLTU header 0xeb90
cltuHeader :: ByteString
cltuHeader = BS.pack [0xeb, 0x90]

-- | The CLTU trailer
{-# INLINABLE cltuTrailer #-}
cltuTrailer :: Int -> ByteString
cltuTrailer n = BS.replicate n 0x55



cltuHeaderParser :: Parser ()
cltuHeaderParser = do
    void $ A.word8 0xEB
    void $ A.word8 0x90

-- | Attoparsec parser for a CLTU.
cltuParser :: Config -> Parser CLTU
cltuParser cfg = do
    let cbSize  = cltuBlockSizeAsWord8 (cfgCltuBlockSize cfg)
        dataLen = fromIntegral (cbSize - 1)

    void $ A.manyTill (A.word8 0x55) cltuHeaderParser

    codeBlocks <- codeBlockParser dataLen

    let proc _ (Left err) = Left err
        proc (bs, parity) (Right cb) =
            case checkCodeBlockParity cbSize bs parity of
                Left  err       -> Left err
                Right dataBlock -> Right (dataBlock : cb)

        checkedCBs = foldr proc (Right mempty) codeBlocks

    case checkedCBs of
        Left  err   -> fail (T.unpack err)
        Right parts -> do
            let
                bs = builderBytes . mconcat . map bytes $ parts
            pure (CLTU bs)




codeBlockParser :: Int -> Parser [(BS.ByteString, Word8)]
codeBlockParser dataLen = go []
  where
    go acc = do
        dat    <- A.take dataLen
        parity <- A.anyWord8
        if BS.all (== 0x55) dat
            then pure (reverse acc)
            else go ((dat, parity) : acc)



-- | Attoparsec parser for a randomized CLTU.
cltuRandomizedParser :: Config -> Parser CLTU
cltuRandomizedParser cfg = do
    let cbSize     = cltuBlockSizeAsWord8 (cfgCltuBlockSize cfg)
        !dataLen   = fromIntegral (cbSize - 1)
        randomizer = initialize (cfgRandomizerStartValue cfg)

    void $ A.manyTill (A.word8 0x55) cltuHeaderParser

    codeBlocks <- codeBlockParser dataLen

    let proc []             acc = pure (Right (reverse acc))
        proc (block : rest) acc = do
            chk <- checkCodeBlockRandomizedParity cbSize block
            case chk of
                Left  err       -> pure (Left err)
                Right dataBlock -> proc rest (dataBlock : acc)

    case evalState (proc codeBlocks []) randomizer of
        Left  err   -> fail (T.unpack err)
        Right parts -> do
            let
                bs = builderBytes . mconcat . map bytes $ parts 
            pure (CLTU bs)


-- | A conduit for decoding CLTUs from a ByteString stream
cltuDecodeC
    :: (MonadReader env m , HasConfig env) =>
    ConduitT BS.ByteString (Either ParseError (PositionRange, CLTU)) m ()
cltuDecodeC = do
    cfg <- view getConfig
    conduitParserEither (cltuParser cfg)

-- | A conduit for decoding randomized CLTUs from a ByteString stream
cltuDecodeRandomizedC
    :: (MonadReader env m , HasConfig env) =>
    ConduitT BS.ByteString (Either ParseError (PositionRange, CLTU)) m ()
cltuDecodeRandomizedC = do
    cfg <- view getConfig
    conduitParserEither (cltuParser cfg)


-- | A conduit for encoding a CLTU in a ByteString for transmission
cltuEncodeC :: (MonadIO m, MonadReader env m , HasConfig env, HasLogFunc env) => ConduitT CLTUInput EncodedCLTU m ()
cltuEncodeC = awaitForever $ \(CLTUInput cltu rqst) -> do
        cfg <- view getConfig
        let encCltu = encode cfg cltu 
            enc = EncodedCLTU encCltu rqst 
        logDebug $ "Encoded CLTU: " <> display (showEncodedCLTU cfg enc)
        yield enc



-- | A conduit for encoding a CLTU in a ByteString for transmission
cltuEncodeRandomizedC :: (MonadIO m, MonadReader env m , HasConfig env, HasLogFunc env) => ConduitT CLTUInput EncodedCLTU m ()
cltuEncodeRandomizedC =
    awaitForever $ \(CLTUInput cltu rqst) -> do
        cfg <- view getConfig
        let encCltu = encodeRandomized cfg cltu
            enc = EncodedCLTU encCltu rqst
        logDebug $ "Encoded Randomized CLTU: " <> display (showEncodedCLTU cfg enc)
        yield enc


{-# INLINABLE encode #-}
-- | Encodes a CLTU into a ByteString suitable for sending via a transport protocol
-- Takes a config as some values of the CLTU ecoding can be specified per mission
-- (e.g. block length of the encoding)
encode :: Config -> CLTU -> ByteString
encode cfg cltu = encodeGeneric cfg cltu encodeCodeBlocks


{-# INLINABLE encodeRandomized #-}
-- | Encodes a CLTU into a ByteString suitable for sending via a transport protocol,
-- but randomizes the data before
-- Takes a config as some values of the CLTU ecoding can be specified per mission
-- (e.g. block length of the encoding)
encodeRandomized :: Config -> CLTU -> ByteString
encodeRandomized cfg cltu = encodeGeneric cfg cltu encodeCodeBlocksRandomized

{-# INLINABLE encodeGeneric #-}
encodeGeneric
    :: Config -> CLTU -> (Config -> ByteString -> Builder) -> ByteString
encodeGeneric cfg (CLTU pl) encoder = builderBytes $ mconcat [bytes cltuHeader, encodedFrame, trailer]
  where
    encodedFrame = encoder cfg pl
    trailer      = bytes $ cltuTrailer
        (fromIntegral (cltuBlockSizeAsWord8 (cfgCltuBlockSize cfg)))




{-# INLINABLE encodeCodeBlocks #-}
-- | Takes a ByteString as payload, splits it into CLTU code blocks according to
-- the configuration, calculates the parity for the code blocks by possibly
-- padding the last code block with the trailer and returns a builder
-- with the result
encodeCodeBlocks :: Config -> ByteString -> Builder
encodeCodeBlocks cfg pl =
    let cbSize = fromIntegral $ cltuBlockSizeAsWord8 (cfgCltuBlockSize cfg) - 1
        blocks = chunkedByBS cbSize pl
        padRight bs =
                let len = BS.length bs
                in  if len < cbSize
                        then BS.append bs (cltuTrailer (cbSize - len))
                        else bs
    in  mconcat $ map (encodeCodeBlock . padRight) blocks

{-# INLINABLE encodeCodeBlocksRandomized #-}
-- | Takes a ByteString as payload, splits it into CLTU code blocks according to
-- the configuration, applies randomization, calculates the parity for the code blocks by possibly
-- padding the last code block with the trailer and returns a builder
-- with the result
encodeCodeBlocksRandomized :: Config -> ByteString -> Builder
encodeCodeBlocksRandomized cfg pl =
    let
        cbSize = fromIntegral $ cltuBlockSizeAsWord8 (cfgCltuBlockSize cfg) - 1
        blocks = map padRight $ chunkedByBS cbSize pl
        randomizer = initialize (cfgRandomizerStartValue cfg)
        padRight bs =
            let len = BS.length bs
            in  if len < cbSize
                    then BS.append bs (cltuTrailer (cbSize - len))
                    else bs

        builderBlocks =
            evalState (mapM encodeCodeBlockRandomized blocks) randomizer
    in
        mconcat builderBlocks


{-# INLINABLE encodeCodeBlock #-}
-- | encodes a single CLTU code block. This function assumes that the given ByteString
-- is already in the correct code block length - 1 (1 byte for parity will be added)
encodeCodeBlock :: ByteString -> Builder
encodeCodeBlock block = bytes block <> word8 (cltuParity block)


{-# INLINABLE encodeCodeBlockRandomized #-}
-- | encodes a single CLTU code block and applies randomization. This function assumes that the given ByteString
-- is already in the correct code block length - 1 (1 byte for parity will be added)
encodeCodeBlockRandomized :: ByteString -> State Randomizer Builder
encodeCodeBlockRandomized block = do
    rblock <- randomize False block
    pure (bytes rblock <> word8 (cltuParity rblock))



{-# INLINABLE cltuParity #-}
-- | calculates the parity of a single code block. The code block is assumed to
-- be of the specified code block length
cltuParity :: ByteString -> Word8
cltuParity !block =
    let
        proc :: Int32 -> Word8 -> Int32
        proc !sreg !octet = fromIntegral $ cltuTable (fromIntegral sreg) octet
        sreg1 = BS.foldl' ({-# SCC proc #-} proc) 0 block
        !result =
            fromIntegral
                $   (({-# SCC sreg1 #-} sreg1 `xor` 0xFF) `shiftL` 1)
                .&. 0xFE
    in
        {-# SCC result #-} result



{-# INLINABLE checkCodeBlockParity #-}
-- | Checks a code block. First, it checks the length against the @expectedLen,
-- then checks the @parity. Returns either an error message or the data block without
-- the parity byte
checkCodeBlockParity :: Word8 -> ByteString -> Word8 -> Either Text ByteString
checkCodeBlockParity expectedLen checkBlock parity =
    let
        len              = BS.length checkBlock + 1
        calculatedParity = cltuParity checkBlock
    in
        if fromIntegral expectedLen /= len
            then Left $ TS.toText
                (  TS.fromText
                      "CLTU block does not have the right length, expected: "
                <> TS.showb expectedLen
                <> TS.fromText " received: "
                <> TS.showb len
                )
            else if calculatedParity == parity
                then
                    Right
                        (if BS.all (== 0x55) checkBlock
                            then BS.empty
                            else checkBlock
                        )
                else if BS.all (== 0x55) checkBlock
                    then Right BS.empty
                    else Left $ TS.toText
                        (  TS.fromText
                              "Error: CLTU code block check failed, calculated: "
                        <> showbHex calculatedParity
                        <> TS.fromText " received: "
                        <> showbHex parity
                        )


{-# INLINABLE checkCodeBlockRandomizedParity #-}
-- | Checks a code block. First, it checks the length against the @expectedLen,
-- then checks the @parity. Returns either an error message or the d-randomized data block without
-- the parity byte.
checkCodeBlockRandomizedParity
    :: Word8 -> (ByteString, Word8) -> State Randomizer (Either Text ByteString)
checkCodeBlockRandomizedParity expectedLen (checkBlock, parity) = do
    let len               = BS.length checkBlock + 1
        !calculatedParity = cltuParity checkBlock

    if fromIntegral expectedLen /= len
        then pure . Left $ TS.toText
            (TS.fromText "CLTU block does not have the right length, expected: "
            <> TS.showb expectedLen
            <> TS.fromText " received: "
            <> TS.showb len
            )
        else if calculatedParity == parity
            then do
                res <- randomize False checkBlock
                pure (Right res)
            else if BS.all (== 0x55) checkBlock
                then pure (Right BS.empty)
                else pure . Left $ TS.toText
                    (  TS.fromText
                          "Error: CLTU code block check failed, calculated: "
                    <> showbHex calculatedParity
                    <> TS.fromText " received: "
                    <> showbHex parity
                    )





