{-# LANGUAGE
    OverloadedStrings
    , BangPatterns
    , NoImplicitPrelude
#-}
module General.GetBitField
    ( getBitField
    )
where


import           RIO
import qualified RIO.ByteString                as B

import           Data.Bits

import           General.Types


getBitField :: ByteString -> BitOffset -> BitSize -> Word64
getBitField bytes bitOff (BitSize nBits) =
    let
        (!idx, !bitNr) = splitBitOffset bitOff
        value1 :: Word64
        !value1 = fromIntegral $ bytes `B.index` idx .&. (255 `shiftR` bitNr)

        sum1    = bitNr + nBits

        loop1 :: Int -> Word64 -> Int -> (Int, Word64, Int)
        loop1 !nb !val !ix
            | nb >= 8
            = let val2 = (val `shiftL` 8) .|. fromIntegral (bytes `B.index` ix)
              in  loop1 (nb - 8) val2 (ix + 1)
            | otherwise
            = (nb, val, ix + 1)
        !value2 = if sum1 < 8
            then value1 `shiftR` (8 - sum1)
            else
                let (!nb, !val, !ix) =
                        loop1 (nBits - (8 - bitNr)) value1 (idx + 1)
                in
                    if nb > 0
                        then
                            (val `shiftL` nb) .|. fromIntegral
                                ((bytes `B.index` ix) `shiftR` (8 - nb))
                        else val
    in
        value2




--     else
--     {
--         nBits -= (8 - bitNr);

--         for(; nBits >= 8; nBits -= 8)
--         {
--             value = (value << 8) | *bytePtr++;
--         }

--         if(nBits > 0)
--         {
--             value = (value << nBits) | (*bytePtr >> (8 - nBits));
--         }
--     }

--     return value;




-- uint    tpktGetBitField(const byte* data, int bitOffset, int bitWidth)
-- {
--     const   byte*   bytePtr = data + bitOffset / 8;
--     int bitNr   = bitOffset % 8;
--     int nBits   = bitWidth;
--     uint    value;

--     value = *bytePtr++ & (255 >> bitNr);

--     if((bitNr + nBits) < 8)
--     {
--         value >>= (8 - (bitNr + nBits));
--     }
--     else
--     {
--         nBits -= (8 - bitNr);

--         for(; nBits >= 8; nBits -= 8)
--         {
--             value = (value << 8) | *bytePtr++;
--         }

--         if(nBits > 0)
--         {
--             value = (value << nBits) | (*bytePtr >> (8 - nBits));
--         }
--     }

--     return value;
-- }
