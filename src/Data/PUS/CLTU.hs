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
    , OverloadedStrings #-}
module Data.PUS.CLTU
    (
        CLTU
        , cltuNew
        , cltuPayLoad
        , encode
        , decode 
        , encodeRandomized
        , decodeRandomized
    )
where


import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import Data.ByteString.Builder
import Data.Word
import Data.Int
import Data.Bits
import Data.Text (Text)
import Data.List (mapAccumL)

import Data.PUS.Config
import Data.PUS.CLTUTable
import Data.PUS.Randomizer

import qualified TextShow as TS
import TextShow.Data.Integral

import General.Chunks



-- The CLTU itself
data CLTU = CLTU {
    -- | returns the actual binary payload data (mostly a TC transfer frame)
    cltuPayLoad :: ByteString
}


{-# INLINABLE cltuNew #-}
-- | create a CLTU from a payload, which will mostly be a TC transfer frame
cltuNew :: ByteString -> CLTU
cltuNew = CLTU

{-# INLINABLE cltuHeader #-}
-- | The CLTU header 0xeb90
cltuHeader :: ByteString
cltuHeader = B.pack [0xeb, 0x90]

-- | The CLTU trailer 
{-# INLINABLE cltuTrailer #-}
cltuTrailer :: Int -> ByteString
cltuTrailer n = B.replicate (fromIntegral n) 0x55


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
encodeGeneric :: Config -> CLTU -> (Config -> ByteString -> Builder) -> ByteString
encodeGeneric cfg (CLTU pl) encoder = toLazyByteString (mconcat [lazyByteString cltuHeader, encodedFrame, trailer])
    where
        encodedFrame = encoder cfg pl
        trailer = encodeCodeBlock (cltuTrailer (fromIntegral (cfgCltuBlockSize cfg - 1)))
        

{-# INLINABLE decode #-}
-- | Decodes incoming data into a CLTU. Returns either an error message or the decoded
-- CLTU itself.
decode :: Config -> ByteString -> Either Text CLTU
decode cfg pl = 
    if cltuHeader `B.isPrefixOf` pl
        then
            let cbSize = cfgCltuBlockSize cfg
                blocks = chunkedBy (fromIntegral cbSize) (B.drop (B.length cltuHeader) pl)

                proc _ (Left err) = Left err
                proc bs (Right cb) = 
                    case checkCodeBlock cbSize bs of
                        Left err -> Left err
                        Right dataBlock -> Right (dataBlock : cb)
                
                checkedCBs = foldr proc (Right mempty) blocks
            in
            case checkedCBs of
                Left err -> Left err
                Right parts -> Right $ CLTU (toLazyByteString . mconcat . map lazyByteString $ parts)
        else
            Left "CLTU Header is missing"


{-# INLINABLE decodeRandomized #-}
-- | Decodes incoming data into a CLTU. Returns either an error message or the decoded
-- CLTU itself.
decodeRandomized :: Config -> ByteString -> Either Text CLTU
decodeRandomized cfg pl = 
    if cltuHeader `B.isPrefixOf` pl
        then
            let cbSize = cfgCltuBlockSize cfg
                blocks = chunkedBy (fromIntegral cbSize) (B.drop (B.length cltuHeader) pl)
                randomizer = initialize (cfgRandomizerStartValue cfg) 

                proc _ [] acc = Right (reverse acc)
                proc r (x : xs) acc = 
                    case checkCodeBlockRandomized r cbSize x of
                        Left err -> Left err
                        Right (newR, block) -> proc newR xs (block : acc)

            in
            case proc randomizer blocks [] of
                Left err -> Left err
                Right parts -> Right $ CLTU (toLazyByteString . mconcat . map lazyByteString $ parts)
        else
            Left "CLTU Header is missing"
            


{-# INLINABLE encodeCodeBlocks #-}
-- | Takes a ByteString as payload, splits it into CLTU code blocks according to 
-- the configuration, calculates the parity for the code blocks by possibly 
-- padding the last code block with the trailer and returns a builder
-- with the result
encodeCodeBlocks :: Config -> ByteString -> Builder
encodeCodeBlocks cfg pl = 
    let cbSize = fromIntegral $ cfgCltuBlockSize cfg - 1
        blocks = chunkedBy cbSize pl
        pad bs = 
            let len = fromIntegral (B.length bs) in
            if len < cbSize then B.append bs (cltuTrailer (cbSize - len)) else bs
    in
    mconcat $ map (encodeCodeBlock . pad) blocks


{-# INLINABLE encodeCodeBlocksRandomized #-}
-- | Takes a ByteString as payload, splits it into CLTU code blocks according to 
-- the configuration, applies randomization, calculates the parity for the code blocks by possibly 
-- padding the last code block with the trailer and returns a builder
-- with the result
encodeCodeBlocksRandomized :: Config -> ByteString -> Builder
encodeCodeBlocksRandomized cfg pl = 
    let cbSize = fromIntegral $ cfgCltuBlockSize cfg - 1
        blocks = map pad $ chunkedBy cbSize pl
        randomizer = initialize (cfgRandomizerStartValue cfg)
        pad bs = 
            let len = fromIntegral (B.length bs) in
            if len < cbSize then B.append bs (cltuTrailer (cbSize - len)) else bs

        (_, builderBlocks) = mapAccumL encodeCodeBlockRandomized randomizer blocks 
    in
    mconcat builderBlocks


{-# INLINABLE encodeCodeBlock #-}
-- | encodes a single CLTU code block. This function assumes that the given ByteString
-- is already in the correct code block length - 1 (1 byte for parity will be added)
encodeCodeBlock :: ByteString -> Builder
encodeCodeBlock block = 
    lazyByteString block <> word8 (cltuParity block)


{-# INLINABLE encodeCodeBlockRandomized #-}
-- | encodes a single CLTU code block and applies randomization. This function assumes that the given ByteString
-- is already in the correct code block length - 1 (1 byte for parity will be added)
encodeCodeBlockRandomized :: Randomizer -> ByteString -> (Randomizer, Builder)
encodeCodeBlockRandomized r block = 
    let (newR, rblock) = randomize r False block
    in
    (newR, lazyByteString rblock <> word8 (cltuParity rblock))
    


{-# INLINABLE cltuParity #-}
-- | calculates the parity of a single code block. The code block is assumed to
-- be of the specified code block length
cltuParity :: ByteString -> Word8 
cltuParity !block =
    let proc :: Word8 -> Int32 -> Int32
        proc !octet !sreg = fromIntegral $ cltuTable (fromIntegral sreg) octet
        sreg1 = B.foldr proc 0 block 
        !result = fromIntegral $ ((sreg1 `xor` 0xFF) `shiftL` 1) .&. 0xFE
    in
    result
        


{-# INLINABLE checkCodeBlock #-}
-- | Checks a code block. First, it checks the length against the expected length,
-- then checks the parity. Returns either an error message or the data block without
-- the parity byte
checkCodeBlock :: Word8 -> ByteString -> Either Text ByteString
checkCodeBlock expectedLen block =
    let len = B.length block
        checkBlock = B.take (len - 1) block
        parity = block `B.index` (len - 1)
        calculatedParity = cltuParity checkBlock
    in
    if fromIntegral expectedLen /= len
        then Left $ TS.toText (TS.fromText "CLTU block does not have the right length, expected: "
            <> TS.showb expectedLen
            <> TS.fromText " received: "
            <> TS.showb len)
        else
            if calculatedParity == parity 
            then Right checkBlock
            else 
                if B.all (== 0x55) checkBlock
                    then Right checkBlock
                    else 
                        Left $ TS.toText (TS.fromText "Error: CLTU code block check failed, calculated: " 
                        <> showbHex calculatedParity 
                        <> TS.fromText " received: " 
                        <> showbHex parity)


{-# INLINABLE checkCodeBlockRandomized #-}
-- | Checks a code block. First, it checks the length against the expected length,
-- then checks the parity. Returns either an error message or the data block without
-- the parity byte
checkCodeBlockRandomized :: Randomizer -> Word8 -> ByteString -> Either Text (Randomizer, ByteString)
checkCodeBlockRandomized r expectedLen block =
    let len = B.length block
        checkBlock = B.take (len - 1) block
        parity = block `B.index` (len - 1)
        calculatedParity = cltuParity checkBlock
    in
    if fromIntegral expectedLen /= len
        then Left $ TS.toText (TS.fromText "CLTU block does not have the right length, expected: "
            <> TS.showb expectedLen
            <> TS.fromText " received: "
            <> TS.showb len)
        else
            if calculatedParity == parity 
            then Right (randomize r False checkBlock)
            else 
                if B.all (== 0x55) checkBlock
                    then Right (r, checkBlock)
                    else 
                        Left $ TS.toText (TS.fromText "Error: CLTU code block check failed, calculated: " 
                        <> showbHex calculatedParity 
                        <> TS.fromText " received: " 
                        <> showbHex parity)





