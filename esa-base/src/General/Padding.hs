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
    )
where

import           RIO
import qualified RIO.ByteString                as B
import qualified Data.ByteString.Char8         as BC


-- | clips a 'ByteString' to the length given in the first parameter
clip :: Int -> ByteString -> ByteString
clip width content =
    if B.length content > width then B.take width content else content


-- | Left pads a 'ByteString'. @pad@ is the value to be used for padding ,
-- @width@ is the desired length of the 'ByteString'. If the 'ByteString' is 
-- already longer than the desired length, it is clipped
leftPadded :: Word8 -> Int -> ByteString -> ByteString
leftPadded pad width content =
    B.replicate (width - B.length content') pad <> content'
    where content' = clip width content

-- | Right pads a 'ByteString'. @pad@ is the value to be used for padding ,
-- @width@ is the desired length of the 'ByteString'. If the 'ByteString' is 
-- already longer than the desired length, it is clipped
rightPadded :: Word8 -> Int -> ByteString -> ByteString
rightPadded pad width content =
    content' <> B.replicate (width - B.length content') pad
    where content' = clip width content

-- | Left pads a 'ByteString'. @pad@ is the value to be used for padding (in this case a 'Char'),
-- @width@ is the desired length of the 'ByteString'. If the 'ByteString' is 
-- already longer than the desired length, it is clipped
leftPaddedC :: Char -> Int -> ByteString -> ByteString
leftPaddedC pad width content =
    BC.replicate (width - B.length content') pad <> content'
    where content' = clip width content

-- | Right pads a 'ByteString'. @pad@ is the value to be used for padding (in this case a 'Char'),
-- @width@ is the desired length of the 'ByteString'. If the 'ByteString' is 
-- already longer than the desired length, it is clipped
rightPaddedC :: Char -> Int -> ByteString -> ByteString
rightPaddedC pad width content =
    content' <> BC.replicate (width - B.length content') pad
    where content' = clip width content

