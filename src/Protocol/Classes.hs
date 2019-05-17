{-# LANGUAGE OverloadedStrings
    , BangPatterns
    , GeneralizedNewtypeDeriving
    , DeriveGeneric
    , RecordWildCards
    , AllowAmbiguousTypes
#-}
module Protocol.Classes
    (
        Header(..)
        , Packet(..)
        , module Protocol.SizeOf
    )
where

import Data.Word

import Protocol.SizeOf


class Header a where
    hdrLength :: Word32


class Packet a where
    packetLength :: a -> Word32