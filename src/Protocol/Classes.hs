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
    )
where

import Data.Word



class Header a where
    hdrLength :: Word32


class Packet a where
    packetLength :: a -> Word32