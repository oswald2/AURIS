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
    , KindSignatures
#-}
module Data.MIB.Types
    (
        PTC(..)
        , PFC(..)
    )
where


import RIO

import Data.Binary
import Data.Aeson
import Codec.Serialise



newtype PTC = PTC Int
    deriving (Eq, Ord, Num, Show, Read, Generic)

instance Binary PTC
instance Serialise PTC
instance ToJSON PTC
instance FromJSON PTC


newtype PFC = PFC Int
    deriving (Eq, Ord, Num, Show, Read, Generic)

instance Binary PFC
instance Serialise PFC
instance ToJSON PFC
instance FromJSON PFC


newtype DefaultTo (a :: Nat) = DefaultTo Int
