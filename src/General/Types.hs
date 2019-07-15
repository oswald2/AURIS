{-# LANGUAGE OverloadedStrings
    , BangPatterns
    , GeneralizedNewtypeDeriving
    , DeriveGeneric
    , RecordWildCards
    , NoImplicitPrelude
    , BinaryLiterals
    , NumericUnderscores
    , FlexibleInstances
    , GADTs
    , ExistentialQuantification
#-}
module General.Types
   (
       Endian(..)
   )
where


import RIO
import Data.Binary
import Data.Aeson
import Codec.Serialise


data Endian = BiE | LiE
    deriving (Eq, Ord, Enum, Show, Read, Generic)


instance Binary Endian
instance Serialise Endian
instance FromJSON Endian
instance ToJSON Endian
