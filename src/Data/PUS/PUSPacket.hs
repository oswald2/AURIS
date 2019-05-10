{-# LANGUAGE
    DeriveGeneric
    , GeneralizedNewtypeDeriving
    , NoImplicitPrelude
#-}
module Data.PUS.PUSPacket
    (
        PUSPacket
        , encodePUSPacket
    )
where


import RIO

import Data.Binary
import Data.Aeson

import GHC.Generics



data PUSPacket = PUSPacket
    deriving (Show, Read, Generic)

instance Binary PUSPacket
instance FromJSON PUSPacket
instance ToJSON PUSPacket where
    toEncoding = genericToEncoding defaultOptions



encodePUSPacket :: PUSPacket -> ByteString
encodePUSPacket _ = undefined
