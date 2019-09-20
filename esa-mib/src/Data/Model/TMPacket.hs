{-# LANGUAGE 
    OverloadedStrings
    , BangPatterns
    , NoImplicitPrelude
#-}
module Data.Model.TMPacket
(
    TMPacket(..)
)
where

import RIO


class TMPacket a where
    getSPID :: a -> Word32