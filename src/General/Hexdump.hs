{-# LANGUAGE OverloadedStrings 
#-}
module General.Hexdump 
    (
        hexdump
    ) 
where
    
    
import Data.Char 
import Data.Word
import Data.Text (Text)
import Data.Text.Lazy.Builder
import qualified Data.ByteString.Lazy as BL
import Data.List (intersperse)
import TextShow

import General.Chunks

import Formatting


hexdump :: BL.ByteString -> Text
hexdump bs = toText result
    where
        chunkSize = 4
        lineChunkSize = 4
        blocks = chunkedBy chunkSize bs 
        groups = chunks lineChunkSize blocks
        addresses :: [Word32]
        addresses = [0, fromIntegral (chunkSize * lineChunkSize) .. ]

        result = mconcat $ zipWith groupToLine addresses groups

        byteToHex :: Word8 -> Builder
        byteToHex x = fromLazyText $ format (left 2 '0' %. hex) x

        byteToChar :: Word8 -> Builder
        byteToChar x = 
            let c = chr (fromIntegral x)
            in 
            if isPrint c 
                then singleton c
                else singleton '.'

        chunkToText = mconcat . map byteToHex . BL.unpack 
        chunkToPrintable = mconcat . map byteToChar . BL.unpack

        groupToLine :: Word32 -> [BL.ByteString] -> Builder
        groupToLine addr line = 
            let content = 
                    fromLazyText (format (left 8 '0' %. hex % ": ") addr)
                        <> mconcat (intersperse (singleton ' ') (map chunkToText line))
                        <> fromLazyText "   "
                        <> mconcat (map chunkToPrintable line)
                        <> singleton '\n'
            in
                content

