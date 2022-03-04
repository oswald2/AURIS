{-|
Module      : General.Padding
Description : Functions for padding byte strings 
Copyright   : (c) Michael Oswald, 2019
License     : BSD-3
Maintainer  : michael.oswald@onikudaki.net
Stability   : experimental
Portability : POSIX

This module provides functions for padding bytestrings to a desired lenth
-}
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
    , General.Padding.padFromRight
    ) where

import           RIO
import qualified RIO.ByteString                as B
import qualified Data.ByteString.Char8         as BC

import           Text.Builder                  as TB


-- | clips a 'ByteString' to the length given in the first parameter
clip :: Int -> ByteString -> ByteString
clip width content =
    if B.length content > width then B.take width content else content


-- | Left pads a 'ByteString'. @padRight@ is the value to be used for padding ,
-- @width@ is the desired length of the 'ByteString'. If the 'ByteString' is 
-- already longer than the desired length, it is clipped
leftPadded :: Word8 -> Int -> ByteString -> ByteString
leftPadded padRight width content =
    B.replicate (width - B.length content') padRight <> content'
    where content' = clip width content

-- | Right pads a 'ByteString'. @padRight@ is the value to be used for padding ,
-- @width@ is the desired length of the 'ByteString'. If the 'ByteString' is 
-- already longer than the desired length, it is clipped
rightPadded :: Word8 -> Int -> ByteString -> ByteString
rightPadded padRight width content =
    content' <> B.replicate (width - B.length content') padRight
    where content' = clip width content

-- | Left pads a 'ByteString'. @padRight@ is the value to be used for padding (in this case a 'Char'),
-- @width@ is the desired length of the 'ByteString'. If the 'ByteString' is 
-- already longer than the desired length, it is clipped
leftPaddedC :: Char -> Int -> ByteString -> ByteString
leftPaddedC padRight width content =
    BC.replicate (width - B.length content') padRight <> content'
    where content' = clip width content

-- | Right pads a 'ByteString'. @padRight@ is the value to be used for padding (in this case a 'Char'),
-- @width@ is the desired length of the 'ByteString'. If the 'ByteString' is 
-- already longer than the desired length, it is clipped
rightPaddedC :: Char -> Int -> ByteString -> ByteString
rightPaddedC padRight width content =
    content' <> BC.replicate (width - B.length content') padRight
    where content' = clip width content


-- | Helper function for 'Text.Builder' as there is only a left padRight function
-- in the library
padFromRight :: Int -> Char -> TB.Builder -> TB.Builder
padFromRight padLen padChar builder =
    let builderLen = TB.length builder
    in  if padLen <= builderLen
            then builder
            else builder
                <> foldMap char (replicate (padLen - builderLen) padChar)
