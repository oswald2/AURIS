{-# LANGUAGE OverloadedStrings
    , BangPatterns
    , GeneralizedNewtypeDeriving
    , DeriveGeneric
    , RecordWildCards
    , NoImplicitPrelude
#-}
module General.Padding
    ( clip
    , leftPadded
    , rightPadded
    , leftPaddedC
    , rightPaddedC
    )
where

import           RIO
import qualified RIO.ByteString                as B
import qualified Data.ByteString.Char8         as BC



clip :: Int -> ByteString -> ByteString
clip width content =
    if B.length content > width then B.take width content else content

leftPadded :: Word8 -> Int -> ByteString -> ByteString
leftPadded pad width content =
    B.replicate (fromIntegral width - B.length content') pad <> content'
    where content' = clip (fromIntegral width) content

rightPadded :: Word8 -> Int -> ByteString -> ByteString
rightPadded pad width content =
    content' <> B.replicate (fromIntegral width - B.length content') pad
    where content' = clip (fromIntegral width) content

leftPaddedC :: Char -> Int -> ByteString -> ByteString
leftPaddedC pad width content =
    BC.replicate (fromIntegral width - B.length content') pad <> content'
    where content' = clip (fromIntegral width) content

rightPaddedC :: Char -> Int -> ByteString -> ByteString
rightPaddedC pad width content =
    content' <> BC.replicate (fromIntegral width - B.length content') pad
    where content' = clip (fromIntegral width) content

