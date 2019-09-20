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
        , DefaultToNothing(..)
    )
where


import RIO
import Data.Hashable

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
    deriving (Show, Read)


instance KnownNat a => FromField (DefaultTo a) where
    parseField s = case runParser (parseField s) of
        Left _ -> pure $ DefaultTo (fromIntegral (natVal (Proxy :: Proxy a)))
        Right n  -> pure $ DefaultTo n

instance ToField (DefaultTo a) where
    toField (DefaultTo n) = toField n



newtype DefaultToNothing a =
    DefaultToNothing (Maybe a)
        deriving (Eq, Ord, Show, Read)
      
instance FromField a => FromField (DefaultToNothing a) where
    parseField s = case runParser (parseField s) of
        Left  _ -> pure $ DefaultToNothing Nothing
        Right n -> pure $ DefaultToNothing n

instance Hashable a => Hashable (DefaultToNothing a) where
    hashWithSalt s (DefaultToNothing d) = s `hashWithSalt` d
        