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
  ( DefaultTo(..)
  , DefaultToNothing(..)
  , CharDefaultTo(..)
  , ShortTextDefaultTo(..)
  )
where


import           RIO
import           Data.Hashable

-- import           Data.Aeson                    as A
-- import           Codec.Serialise
import           Data.Csv
import           Data.Text.Short (ShortText)
import qualified Data.Text.Short as ST

import           GHC.TypeLits




newtype DefaultTo (a :: Nat) = DefaultTo { getDefaultInt :: Int }
    deriving (Eq, Show, Read)


instance KnownNat a => FromField (DefaultTo a) where
  parseField s = case runParser (parseField s) of
    Left  _ -> pure $ DefaultTo (fromIntegral (natVal (Proxy :: Proxy a)))
    Right n -> pure $ DefaultTo n

instance ToField (DefaultTo a) where
  toField (DefaultTo n) = toField n


newtype CharDefaultTo (a :: Symbol) = CharDefaultTo { getDefaultChar :: Char }
    deriving (Eq, Show, Read)

instance KnownSymbol a => FromField (CharDefaultTo a) where
  parseField s = case runParser (parseField s) of
    Left _ -> pure $ CharDefaultTo $ case symbolVal (Proxy :: Proxy a) of
      ""      -> 'N'
      (x : _) -> x
    Right x -> pure $ CharDefaultTo x

instance ToField (CharDefaultTo a) where
  toField (CharDefaultTo x) = toField x


newtype ShortTextDefaultTo (a :: Symbol) = ShortTextDefaultTo { getDefaultShortText :: ShortText }
    deriving (Eq, Show, Read)

instance KnownSymbol a => FromField (ShortTextDefaultTo a) where
  parseField s = case runParser (parseField s) of
    Left  _ -> pure $ ShortTextDefaultTo (ST.fromString (symbolVal (Proxy :: Proxy a)))
    Right x -> pure $ ShortTextDefaultTo x

instance ToField (ShortTextDefaultTo a) where
  toField (ShortTextDefaultTo x) = toField x


newtype DefaultToNothing a =
    DefaultToNothing (Maybe a)
        deriving (Eq, Ord, Show, Read)

instance FromField a => FromField (DefaultToNothing a) where
  parseField s = case runParser (parseField s) of
    Left  _ -> pure $ DefaultToNothing Nothing
    Right n -> pure $ DefaultToNothing n

instance Hashable a => Hashable (DefaultToNothing a) where
  hashWithSalt s (DefaultToNothing d) = s `hashWithSalt` d

