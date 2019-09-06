{-# LANGUAGE OverloadedStrings
    , BangPatterns
    , GeneralizedNewtypeDeriving
    , DeriveGeneric
    , RecordWildCards
    , NumericUnderscores
    , FlexibleInstances
    , NoImplicitPrelude
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
        , TimeRepConversion(..)
        , timeToWord64'
        , word64ToTime'
        , timeToMicro'
        , microToTime'
    )
where

import RIO

import Data.Coerce
import Data.Bits


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


microSecs :: Integer
microSecs = 1_000_000


-- | This class is for handling time conversions of the different time types
-- | This can be used in encoding as well as for displaying stuff, so
-- | this is a bit more general than it looks like
class TimeRepConversion a where
    -- | converts time to a 64 bit word. High word are seconds, low word are
    -- | microseconds
    timeToWord64 :: a -> Word64
    word64ToTime :: Word64 -> Bool -> a

    -- | converts a time to microseconds in a 64 bit word since the epoch
    timeToMicro :: a -> Integer
    microToTime :: Integer -> Bool -> a




{-# INLINABLE timeToWord64' #-}
timeToWord64' :: Integer -> Int32 -> Bool -> Word64
timeToWord64' sec usec _ =
    let sec' :: Int64
        sec' = fromIntegral sec `shiftL` 32
    in  fromIntegral sec' .|. (fromIntegral usec .&. 0xFFFFFFFF)

{-# INLINABLE word64ToTime' #-}
word64ToTime' :: Word64 -> (Integer, Int32)
word64ToTime' val' =
    let val :: Int64
        val = fromIntegral val'
        sec = val `shiftR` 32
        usec :: Int32
        usec = fromIntegral (val .&. 0xFFFFFFFF)
    in  (fromIntegral sec, usec)


{-# INLINABLE timeToMicro' #-}
timeToMicro' :: Integer -> Int32 -> Bool -> Integer
timeToMicro' sec usec _
    = let sign = if sec < 0 || usec < 0 then (-1) else 1
        in
            sign
                * (abs sec
                * microSecs
                + fromIntegral (abs usec))


{-# INLINABLE microToTime' #-}
microToTime' :: Integer -> (Integer, Int32)
microToTime' x =
    let sign        = signum x
        absx        = abs x
        (sec, usec) = absx `quotRem` microSecs
    in  (sign * sec, fromIntegral (sign * usec))
