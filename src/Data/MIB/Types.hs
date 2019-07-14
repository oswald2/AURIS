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
    , DataKinds
    , ScopedTypeVariables
#-}
module Data.MIB.Types
    (
        PTC(..)
        , PFC(..)
        , DefaultTo(..)
    )
where


import RIO

import Data.Binary
import Data.Aeson
import Codec.Serialise
import Data.Csv

import GHC.TypeLits



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



instance KnownNat a => FromField (DefaultTo a) where
    parseField s = case runParser (parseField s) of
        Left _ -> pure $ DefaultTo (fromIntegral (natVal (Proxy :: Proxy a)))
        Right n  -> pure $ DefaultTo n

instance ToField (DefaultTo a) where
    toField (DefaultTo n) = toField n
