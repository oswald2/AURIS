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
module General.TimeSpan
  ( TimeSpan(..)
  , TimeSpn
  , ToTimeSpan(..)
  , mkTimeSpan
  , getMicro
  , Hours(..)
  , Minutes(..)
  , Seconds(..)
  , MilliSeconds(..)
  , MicroSeconds(..)
  , toMilliSeconds
  , toSeconds
  , toMinutes
  , toHours
  )
where

import           RIO
import           Data.Coerce
import           Codec.Serialise
import           Data.Aeson


data Hours = Hours deriving Generic
data Minutes = Minutes  deriving Generic
data Seconds = Seconds deriving Generic
data MilliSeconds = MilliSeconds deriving Generic
data MicroSeconds = MicroSeconds deriving Generic

instance Serialise Hours
instance Serialise Minutes
instance Serialise Seconds
instance Serialise MilliSeconds
instance Serialise MicroSeconds

instance FromJSON Hours
instance FromJSON Minutes
instance FromJSON Seconds
instance FromJSON MilliSeconds
instance FromJSON MicroSeconds

instance ToJSON Hours
instance ToJSON Minutes
instance ToJSON Seconds
instance ToJSON MilliSeconds
instance ToJSON MicroSeconds


-- | We store the time internally in micro seconds precision
newtype TimeSpn a = TimeSpn { getSpan :: Int64 }
    deriving (Eq, Ord, Num, Show, Read, Generic)

class ToMicro a where
    toMicro :: a -> Double -> Int64


instance ToMicro MicroSeconds where
  toMicro _ = truncate
instance ToMicro MilliSeconds where
  toMicro _ val = truncate (val * 1_000)
instance ToMicro Seconds where
  toMicro _ val = truncate (val * 1_000_000)
instance ToMicro Minutes where
  toMicro _ val = truncate (val * 60 * 1_000_000)
instance ToMicro Hours where
  toMicro _ val = truncate (val * 3600 * 1_000_000)

instance Serialise a => Serialise (TimeSpn a)
instance FromJSON a => FromJSON (TimeSpn a)
instance ToJSON a => ToJSON (TimeSpn a) where
  toEncoding = genericToEncoding defaultOptions

mkTimeSpan :: ToMicro a => a -> Double -> TimeSpn a
mkTimeSpan t val = TimeSpn (toMicro t val)

getMicro :: TimeSpn a -> Int64
getMicro (TimeSpn val) = val


newtype TimeSpan = TimeSpan Int64
    deriving (Eq, Ord, Num, Show, Read, Generic)

class ToTimeSpan a where
    toTimeSpan :: a -> TimeSpan

instance ToTimeSpan (TimeSpn MicroSeconds) where
  toTimeSpan = coerce
instance ToTimeSpan (TimeSpn MilliSeconds) where
  toTimeSpan = coerce
instance ToTimeSpan (TimeSpn Seconds) where
  toTimeSpan = coerce
instance ToTimeSpan (TimeSpn Minutes) where
  toTimeSpan = coerce
instance ToTimeSpan (TimeSpn Hours) where
  toTimeSpan = coerce

instance Serialise TimeSpan

toMilliSeconds :: TimeSpan -> TimeSpn MilliSeconds
toMilliSeconds = coerce

toSeconds :: TimeSpan -> TimeSpn Seconds
toSeconds = coerce

toMinutes :: TimeSpan -> TimeSpn Minutes
toMinutes = coerce

toHours :: TimeSpan -> TimeSpn Hours
toHours = coerce
