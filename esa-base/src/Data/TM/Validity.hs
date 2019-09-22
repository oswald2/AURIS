{-# LANGUAGE 
    OverloadedStrings
    , BangPatterns
    , NoImplicitPrelude
#-}
module Data.TM.Validity 
(
    Validity
    , isValid
)
where

import RIO



newtype Validity = Validity { getRawValidity :: Word32 }
    deriving (Eq, Show, Read)


isValid :: Validity -> Bool 
isValid (Validity x) = x == 0

