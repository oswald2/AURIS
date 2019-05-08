{-# LANGUAGE OverloadedStrings
    , BangPatterns
    , GeneralizedNewtypeDeriving
    , DeriveGeneric
    , RecordWildCards
    , NumericUnderscores
#-}
module Data.PUS.Time
    (
        TimeSpan(..)
        , mkTimeSpan
        , getMicro
        , Hours(..)
        , Minutes(..)
        , Seconds(..)
        , MilliSeconds(..)
        , MicroSeconds(..)
    )
where

data Hours = Hours
data Minutes = Minutes
data Seconds = Seconds
data MilliSeconds = MilliSeconds
data MicroSeconds = MicroSeconds

-- | We store the time internally in micro seconds precision
newtype TimeSpan a = TimeSpan { getSpan :: Integer }
    deriving (Eq, Ord, Num, Show, Read)

class ToMicro a where
    toMicro :: a -> Double -> Integer


instance ToMicro MicroSeconds where
    toMicro _ val = truncate val
instance ToMicro MilliSeconds where
    toMicro _ val = truncate (val * 1_000)
instance ToMicro Seconds where
    toMicro _ val = truncate (val * 1_000_000)
instance ToMicro Minutes where
    toMicro _ val = truncate (val * 60 * 1_000_000)
instance ToMicro Hours where
    toMicro _ val = truncate (val * 3600 * 1_000_000)
                            
   
mkTimeSpan :: ToMicro a => a -> Double -> TimeSpan a
mkTimeSpan t val = TimeSpan (toMicro t val)

getMicro :: TimeSpan a -> Integer
getMicro (TimeSpan val) = val


newtype TimeSpanInternal = TimeSpanInternal Integer
    deriving (Eq, Ord, Num)

