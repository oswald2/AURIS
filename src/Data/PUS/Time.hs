{-# LANGUAGE OverloadedStrings
    , BangPatterns
    , GeneralizedNewtypeDeriving
    , DeriveGeneric
    , RecordWildCards
    , NumericUnderscores
    , FlexibleInstances
#-}
module Data.PUS.Time
    (
        TimeSpan(..)
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

import Data.Coerce

data Hours = Hours
data Minutes = Minutes
data Seconds = Seconds
data MilliSeconds = MilliSeconds
data MicroSeconds = MicroSeconds

-- | We store the time internally in micro seconds precision
newtype TimeSpn a = TimeSpn { getSpan :: Integer }
    deriving (Eq, Ord, Num, Show, Read)

class ToMicro a where
    toMicro :: a -> Double -> Integer


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



mkTimeSpan :: ToMicro a => a -> Double -> TimeSpn a
mkTimeSpan t val = TimeSpn (toMicro t val)

getMicro :: TimeSpn a -> Integer
getMicro (TimeSpn val) = val


newtype TimeSpan = TimeSpan Integer
    deriving (Eq, Ord, Num, Show, Read)

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

toMilliSeconds :: TimeSpan -> TimeSpn MilliSeconds
toMilliSeconds = coerce

toSeconds :: TimeSpan -> TimeSpn Seconds
toSeconds = coerce

toMinutes :: TimeSpan -> TimeSpn Minutes
toMinutes = coerce

toHours :: TimeSpan -> TimeSpn Hours
toHours = coerce
