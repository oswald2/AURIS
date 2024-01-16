{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

{- |
Module      : General.Time
Description : General types for time handling
Copyright   : (c) Michael Oswald, 2019
License     : BSD-3
Maintainer  : michael.oswald@onikudaki.net
Stability   : experimental
Portability : POSIX

This module provides data types and functions for handling time values
within the system.

The time handling is based on 3 layers:

  1. the 'SunTime' handles the normal time at application level (user facing)
  2. the 'EpochTime' is for converting between encoded times and 'SunTime' with
     specifiable epochs. Each mission can have it's own epoch
  3. the encoded times. There are various formats how the time can be encoded/decoded.
     See the module "Data.PUS.EncTime" for details.
-}
module General.Time (
    TimeRepConversion (..),
    timeToWord64',
    word64ToTime',
    timeToMicro',
    microToTime',
    SunTime,
    makeTime,
    fromMilli,
    tdsSecs,
    tdsMicro,
    nullTime,
    nullRelTime,
    oneMicroSecond,
    oneSecond,
    oneHour,
    tdsNull,
    secsInDay,
    secsInYear,
    milliSecsInDay,
    microSecsInDay,
    microSecInt,
    microSecond,
    displayRaw,
    displayISO,
    displayDouble,
    timeToComponents,
    timeFromComponents,
    General.Time.getCurrentTime,
    fromDouble,
    DeltaTime (..),
    edenTime,
    edenTimeParser,
    fromText,
    sunTimeParser,
    (<+>),
    (<->),
    addTimes,
    subTimes,
    addSpan,
    subSpan,
    negTime,
    LeapSeconds (..),
    CorrelationCoefficients,
    createCoeffs,
    defaultCoeffs,
    mcsTimeToOBT,
    obtToMcsTim,
    uttObtLeap,
    obtUttLeap,
    makeEpoch,
    getEpoch,
    nullEpochTime,
    nullGPSTime,
    nullTAITime,
    nullEpochTimeRel,
    nullGPSTimeRel,
    nullTAITimeRel,
    oneMicroSecondEpoch,
    oneMicroSecondGPS,
    defaultEpoch,
    epoch1958,
    epochGPS,
    epochUnix,
    epoch2000,
    epochTimeToSunTime,
    sunTimeToEpochTime,
    Epoch,
    FreeEpoch (..),
    EpochType (..),
    EpochTime (..),
    displayTimeMilli,
    toUTCTime,
    displayUTCTimeMilli,
    utcTimeToComponents,
    fromUTC,
    fromSystemTime,
    toDouble,
) where

import RIO
import RIO.Partial (read)
import qualified RIO.Text as T

import Codec.Serialise

import Data.Aeson
import Data.Bits
import Data.Time
import Data.Time.Calendar.OrdinalDate
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Time.Clock.System

import qualified Text.Builder as TB

import Text.Megaparsec
import Text.Megaparsec.Char

import qualified Data.Attoparsec.ByteString as A
import qualified Data.Attoparsec.ByteString.Char8 as A

import General.TimeSpan
import General.Types (ToDouble (..))

type Parser = Parsec Void Text

-- | one micro second
{-# INLINEABLE microSecs #-}
microSecs :: Int64
microSecs = 1_000_000

{- | This class is for handling time conversions of the different time types
This can be used in encoding as well as for displaying stuff, so
this is a bit more general than it looks like
-}
class TimeRepConversion a where
    -- | converts time to a 64 bit word. High word are seconds, low word are
    -- | microseconds
    timeToWord64 :: a -> Word64

    word64ToTime :: Word64 -> Bool -> a

    -- | converts a time to microseconds in a 64 bit word since the epoch
    timeToMicro :: a -> Int64

    microToTime :: Int64 -> Bool -> a

{-# INLINEABLE timeToWord64' #-}
timeToWord64' :: Int64 -> Int32 -> Bool -> Word64
timeToWord64' sec usec _ =
    let sec' :: Int64
        sec' = sec `shiftL` 32
     in fromIntegral sec' .|. (fromIntegral usec .&. 0xFFFFFFFF)

{-# INLINEABLE word64ToTime' #-}
word64ToTime' :: Word64 -> (Int64, Int32)
word64ToTime' val' =
    let val :: Int64
        val = fromIntegral val'
        sec = val `shiftR` 32
        usec :: Int32
        usec = fromIntegral (val .&. 0xFFFFFFFF)
     in (sec, usec)

{-# INLINEABLE timeToMicro' #-}
timeToMicro' :: Int64 -> Int32 -> Bool -> Int64
timeToMicro' sec usec _ =
    let sign = if sec < 0 || usec < 0 then (-1) else 1
     in sign * (abs (sec) * microSecs + fromIntegral (abs usec))

{-# INLINEABLE microToTime' #-}
microToTime' :: Int64 -> (Int64, Int32)
microToTime' x =
    let sign = signum x
        absx = abs x
        (sec, usec) = absx `quotRem` microSecs
     in (sign * sec, fromIntegral (sign * usec))

{- | The basic sun data type. The sun time  is the general class on application
level which is used in the system. It is used in all user interface interactions
and is subject to time correlation and leap second correction (currently fixed leap
seconds taken from the configuration).
-}
data SunTime = SunTime
    { tdsTime :: !Int64
    , tdsDelta :: !Bool
    }
    deriving (Eq, Show, Read, Generic)

instance NFData SunTime
instance Serialise SunTime
instance FromJSON SunTime
instance ToJSON SunTime where
    toEncoding = genericToEncoding defaultOptions

fromUTC :: UTCTime -> SunTime
fromUTC utcTime = fromSystemTime (utcToSystemTime utcTime)

fromSystemTime :: SystemTime -> SunTime
fromSystemTime t =
    let val =
            systemSeconds t
                * 1_000_000
                + fromIntegral (systemNanoseconds t)
                `div` 1_000
     in SunTime val False

{- | converts the time into a 'Double' represinting the seconds
from the Unix epoch. The fractional part are the subseconds
-}
instance ToDouble SunTime where
    toDouble (SunTime msecs _) = fromIntegral msecs / 1_000_000

{- | Convert from a double to a time. The double is assumed to
be in seconds
-}
{-# INLINEABLE fromDouble #-}
fromDouble :: Double -> Bool -> SunTime
fromDouble secs = SunTime (round (secs * 1_000_000))

{-# INLINEABLE fromMilli #-}
fromMilli :: Int64 -> Bool -> SunTime
fromMilli milli = SunTime (milli * 1000)

instance Ord SunTime where
    SunTime t1 False `compare` SunTime t2 False = t1 `compare` t2
    SunTime t1 True `compare` SunTime t2 True = t1 `compare` t2
    SunTime _t1 False `compare` SunTime _t2 True = GT
    SunTime _t1 True `compare` SunTime _t2 False = LT

{- | Create a 'SunTime' out of seconds, microseconds and if it is a
delta time (True) or not
-}
{-# INLINEABLE makeTime #-}
makeTime :: Int64 -> Int32 -> Bool -> SunTime
makeTime sec usec delta =
    let (restsec, usec') = abs usec `quotRem` fromIntegral microSecInt
        sign = if sec < 0 || usec < 0 then (-1) else 1
        newSec = (abs sec + fromIntegral restsec)
        newMicro = newSec * microSecInt + fromIntegral usec'
     in SunTime (sign * newMicro) delta

-- | returns the seconds of the 'SunTime'
{-# INLINEABLE tdsSecs #-}
tdsSecs :: SunTime -> Int64
tdsSecs (SunTime micro _) = micro `quot` microSecInt

-- | returns the micro seconds of the 'SunTime'
{-# INLINEABLE tdsMicro #-}
tdsMicro :: SunTime -> Int32
tdsMicro (SunTime micro _) = fromIntegral (micro `rem` microSecInt)

-- | the null time for standard unix time
{-# INLINEABLE nullTime #-}
nullTime :: SunTime
nullTime = SunTime 0 False

-- | a null relative time
{-# INLINEABLE nullRelTime #-}
nullRelTime :: SunTime
nullRelTime = SunTime 0 True

-- | one micro second
{-# INLINEABLE oneMicroSecond #-}
oneMicroSecond :: SunTime
oneMicroSecond = SunTime 1 True

-- | one second
{-# INLINEABLE oneSecond #-}
oneSecond :: SunTime
oneSecond = SunTime 1_000_000 True

{-# INLINEABLE oneHour #-}
oneHour :: SunTime
oneHour = SunTime (3600 * 1_000_000) True

-- | check if a time is a null time
{-# INLINEABLE tdsNull #-}
tdsNull :: SunTime -> Bool
tdsNull (SunTime mic _) = mic == 0

-- | the seconds per day (no leaps)
{-# INLINEABLE secsInDay #-}
secsInDay :: Int64
secsInDay = 86400

-- | the milli seconds in a day (no leaps)
{-# INLINEABLE milliSecsInDay #-}
milliSecsInDay :: Int64
milliSecsInDay = secsInDay * 1000

-- | the micro seconds in a day (no leaps)
{-# INLINEABLE microSecsInDay #-}
microSecsInDay :: Int64
microSecsInDay = secsInDay * 1_000_000

-- | the micro seconds per seoncd as 'Int64'
{-# INLINEABLE microSecInt #-}
microSecInt :: Int64
microSecInt = 1_000_000

-- | the seconds in a year (no leaps)
{-# INLINEABLE secsInYear #-}
secsInYear :: Int64
secsInYear = 31536000

-- | the micro seconds per second as a 'Double'
{-# INLINEABLE microSecond #-}
microSecond :: Double
microSecond = fromIntegral microSecInt

-- | Returns the current system time as a 'SunTime'
{-# INLINEABLE getCurrentTime #-}
getCurrentTime :: IO SunTime
getCurrentTime = do
    t <- Data.Time.Clock.getCurrentTime
    let ts :: Double = realToFrac (utcTimeToPOSIXSeconds t) * 1_000_000
    return (SunTime (round ts) False)

-- | show the low level internal data of a time
{-# INLINEABLE displayRaw #-}
displayRaw :: SunTime -> Text
displayRaw (SunTime mic delta) =
    utf8BuilderToText
        ("SunTime " <> displayShow mic <> " " <> displayShow delta)

toUTCTime :: SunTime -> UTCTime
toUTCTime (SunTime t _) = posixSecondsToUTCTime (realToFrac t / 1_000_000)

-- | Display a 'SunTime' in ISO format
{-# INLINEABLE displayISO #-}
displayISO :: SunTime -> Text
displayISO ts@(SunTime _ False) =
    let (yr, _doy, mn, dom, hh, mm, ss, micro) = timeToComponents ts
     in TB.run
            $ TB.padFromLeft 4 '0' (TB.decimal yr)
            <> TB.char '-'
            <> TB.padFromLeft 2 '0' (TB.decimal mn)
            <> TB.char '-'
            <> TB.padFromLeft 2 '0' (TB.decimal dom)
            <> TB.char 'T'
            <> TB.padFromLeft 2 '0' (TB.decimal hh)
            <> TB.char '.'
            <> TB.padFromLeft 2 '0' (TB.decimal mm)
            <> TB.char '.'
            <> TB.padFromLeft 2 '0' (TB.decimal ss)
            <> TB.char '.'
            <> TB.padFromLeft 6 '0' (TB.decimal micro)
displayISO ts@(SunTime t True) =
    let (yr, _doy, mn, dom, hh, mm, ss, micro) = timeToComponents ts
        sign = if t < 0 then '-' else '+'
     in TB.run
            $ TB.char sign
            <> TB.padFromLeft 4 '0' (TB.decimal yr)
            <> TB.char '-'
            <> TB.padFromLeft 2 '0' (TB.decimal mn)
            <> TB.char '-'
            <> TB.padFromLeft 2 '0' (TB.decimal dom)
            <> TB.char 'T'
            <> TB.padFromLeft 2 '0' (TB.decimal hh)
            <> TB.char '.'
            <> TB.padFromLeft 2 '0' (TB.decimal mm)
            <> TB.char '.'
            <> TB.padFromLeft 2 '0' (TB.decimal ss)
            <> TB.char '.'
            <> TB.padFromLeft 6 '0' (TB.decimal micro)

displayTimeMilli :: SunTime -> Text
displayTimeMilli ts@(SunTime _t False) =
    let (yr, doy, _mn, _dom, hh, mm, ss, micro) = timeToComponents ts
     in TB.run
            $ TB.padFromLeft 4 '0' (TB.decimal yr)
            <> TB.char '.'
            <> TB.padFromLeft 3 '0' (TB.decimal doy)
            <> TB.char '.'
            <> TB.padFromLeft 2 '0' (TB.decimal hh)
            <> TB.char '.'
            <> TB.padFromLeft 2 '0' (TB.decimal mm)
            <> TB.char '.'
            <> TB.padFromLeft 2 '0' (TB.decimal ss)
            <> TB.char '.'
            <> TB.padFromLeft 3 '0' (TB.decimal micro)
displayTimeMilli tt =
    let secs = sec `rem` 60
        mins = sec `quot` 60 `rem` 60
        hours = sec `quot` 3600 `rem` 24
        days = sec `quot` 86400 `rem` 365
        years = sec `quot` (86400 * 365)
        mic = tdsMicro tt
        sec = tdsSecs tt
        sign = if sec < 0 then '-' else '+'
     in TB.run
            $ TB.char sign
            <> TB.padFromLeft 4 '0' (TB.decimal years)
            <> TB.char '.'
            <> TB.padFromLeft 3 '0' (TB.decimal days)
            <> TB.char '.'
            <> TB.padFromLeft 2 '0' (TB.decimal hours)
            <> TB.char '.'
            <> TB.padFromLeft 2 '0' (TB.decimal mins)
            <> TB.char '.'
            <> TB.padFromLeft 2 '0' (TB.decimal secs)
            <> TB.char '.'
            <> TB.padFromLeft 3 '0' (TB.decimal ((abs mic) `quot` 1000))

timeToComponents :: SunTime -> (Integer, Int, Int, Int, Int, Int, Int, Int)
timeToComponents (SunTime t _) =
    let (_secs, micro) = t `quotRem` 1_000_000
        ts = realToFrac t / 1_000_000
        LocalTime day (TimeOfDay hh mm ss) =
            utcToLocalTime utc (posixSecondsToUTCTime ts)
        (yr, mn, dom) = toGregorian day
        (_, doy) = toOrdinalDate day
        sec = truncate ss
     in (yr, doy, mn, dom, hh, mm, sec, fromIntegral micro)

utcTimeToComponents :: UTCTime -> (Integer, Int, Int, Int, Int, Int, Int, Int)
utcTimeToComponents utcTime =
    let
        LocalTime day (TimeOfDay hh mm ss) = utcToLocalTime utc utcTime
        (yr, mn, dom) = toGregorian day
        (_, doy) = toOrdinalDate day
        sec = truncate ss
        pico =
            diffTimeToPicoseconds (utctDayTime utcTime) `rem` 1_000_000_000_000
        micro = pico `div` 1_000_000
     in
        (yr, doy, mn, dom, hh, mm, sec, fromIntegral micro)

displayUTCTimeMilli :: UTCTime -> Text
displayUTCTimeMilli utcTime =
    let (yr, doy, _mn, _dom, hh, mm, ss, micro) = utcTimeToComponents utcTime
     in TB.run
            $ TB.padFromLeft 4 '0' (TB.decimal yr)
            <> TB.char '.'
            <> TB.padFromLeft 3 '0' (TB.decimal doy)
            <> TB.char '.'
            <> TB.padFromLeft 2 '0' (TB.decimal hh)
            <> TB.char '.'
            <> TB.padFromLeft 2 '0' (TB.decimal mm)
            <> TB.char '.'
            <> TB.padFromLeft 2 '0' (TB.decimal ss)
            <> TB.char '.'
            <> TB.padFromLeft 3 '0' (TB.decimal micro)

-- timeFromComponents
--     :: Year -> DayOfYear -> Hour -> Minute -> Int -> Int -> SunTime
timeFromComponents :: Int -> Int -> Int -> Int -> Int -> Int -> SunTime
timeFromComponents y d h m s micro =
    let sec = daySegmToSeconds y d h m s
     in makeTime sec (fromIntegral micro) False

instance Display SunTime where
    -- \| display a 'SunTime' in SCOS format (with day of year)
    textDisplay t1@(SunTime _t False) =
        let (yr, doy, _mn, _dom, hh, mm, ss, micro) = timeToComponents t1
         in TB.run
                $ TB.padFromLeft 4 '0' (TB.decimal yr)
                <> TB.char '.'
                <> TB.padFromLeft 3 '0' (TB.decimal doy)
                <> TB.char '.'
                <> TB.padFromLeft 2 '0' (TB.decimal hh)
                <> TB.char '.'
                <> TB.padFromLeft 2 '0' (TB.decimal mm)
                <> TB.char '.'
                <> TB.padFromLeft 2 '0' (TB.decimal ss)
                <> TB.char '.'
                <> TB.padFromLeft 6 '0' (TB.decimal micro)
    textDisplay tt =
        let secs = sec `rem` 60
            mins = sec `quot` 60 `rem` 60
            hours = sec `quot` 3600 `rem` 24
            days = sec `quot` 86400 `rem` 365
            years = sec `quot` (86400 * 365)
            mic = tdsMicro tt
            sec = tdsSecs tt
            sign = if sec < 0 then '-' else '+'
         in TB.run
                $ TB.char sign
                <> TB.padFromLeft 4 '0' (TB.decimal years)
                <> TB.char '.'
                <> TB.padFromLeft 3 '0' (TB.decimal days)
                <> TB.char '.'
                <> TB.padFromLeft 2 '0' (TB.decimal hours)
                <> TB.char '.'
                <> TB.padFromLeft 2 '0' (TB.decimal mins)
                <> TB.char '.'
                <> TB.padFromLeft 2 '0' (TB.decimal secs)
                <> TB.char '.'
                <> TB.padFromLeft 6 '0' (TB.decimal (abs mic))

-- | Display a SunTime as a floating point value (in seconds)
{-# INLINEABLE displayDouble #-}
displayDouble :: SunTime -> Text
displayDouble t = utf8BuilderToText $ displayShow (toDouble t) <> " s"

{- | Convert a 'SunTime' into a binary 'ByteString' in the format for the EDEN
protocol
-}
{-# INLINEABLE edenTime #-}
edenTime :: SunTime -> ByteString
edenTime t =
    let (yr, doy, _mn, _dom, hh, mm, ss, micro) = timeToComponents t
        str =
            TB.run
                $ TB.padFromLeft 4 '0' (TB.decimal yr)
                <> TB.char ' '
                <> TB.padFromLeft 3 '0' (TB.decimal doy)
                <> TB.char ':'
                <> TB.padFromLeft 2 '0' (TB.decimal hh)
                <> TB.char ':'
                <> TB.padFromLeft 2 '0' (TB.decimal mm)
                <> TB.char ':'
                <> TB.padFromLeft 2 '0' (TB.decimal ss)
                <> TB.char '.'
                <> TB.padFromLeft 3 '0' (TB.decimal micro)
                <> TB.char ' '
     in encodeUtf8 str

{-# INLINEABLE edenTimeParser #-}
edenTimeParser :: A.Parser SunTime
edenTimeParser = do
    years <- A.decimal
    void $ A.char ' '
    days <- A.decimal
    void $ A.char ':'
    hours <- A.decimal
    void $ A.char ':'
    minutes <- A.decimal
    void $ A.char ':'
    seconds <- A.decimal
    void $ A.char '.'
    subSeconds <- A.decimal
    void $ A.char ' '

    let sec = daySegmToSeconds years days hours minutes seconds

    return $ makeTime sec (subSeconds * 1000) False

{- | parses a 'Text' and returns a 'SunTime' time. The format is standard SCOS
format YYY.DDD.hh.mm.ss.mmmmmm
-}
fromText :: Text -> Either Text SunTime
fromText str = case parse sunTimeParser "" str of
    Left err -> Left (T.pack (show err))
    Right x -> Right x

-- | the time parser
sunTimeParser :: Parser SunTime
sunTimeParser = do
    Text.Megaparsec.Char.space
    -- option 0 sign
    sign <-
        optional
            ( choice
                [Text.Megaparsec.Char.char '-', Text.Megaparsec.Char.char '+']
            )

    case sign of
        Just si -> do
            det <- eitherP (count 3 digitChar) (count 2 digitChar)
            case det of
                Left days' -> do
                    let year = 0
                        day = read days'
                    void $ Text.Megaparsec.Char.char '.'
                    hour <- read <$> count 2 digitChar
                    void $ Text.Megaparsec.Char.char '.'
                    minute <- read <$> count 2 digitChar
                    void $ Text.Megaparsec.Char.char '.'
                    second' <- read <$> count 2 digitChar
                    sub <- optional subsecs

                    let secRel sgn =
                            daysSegmToSecondsRel
                                sgn
                                year
                                day
                                hour
                                minute
                                second'
                        (sec', neg) = secRel si
                        time = makeTime sec' (fromMaybe 0 sub) True

                    if neg then return (negTime time) else return time
                Right hour' -> do
                    void $ Text.Megaparsec.Char.char '.'
                    minute <- read <$> count 2 digitChar
                    void $ Text.Megaparsec.Char.char '.'
                    second' <- read <$> count 2 digitChar
                    sub <- optional subsecs

                    let year = 0
                        day = 0
                        hour = read hour'
                        secRel sgn =
                            daysSegmToSecondsRel
                                sgn
                                year
                                day
                                hour
                                minute
                                second'
                        (sec', neg) = secRel si
                        time = makeTime sec' (fromMaybe 0 sub) True

                    if neg then return (negTime time) else return time
        Nothing -> do
            year <- read <$> Text.Megaparsec.some digitChar
            void $ Text.Megaparsec.Char.char '.'
            day <- read <$> count 3 digitChar
            void $ Text.Megaparsec.Char.char '.'
            hour <- read <$> count 2 digitChar
            void $ Text.Megaparsec.Char.char '.'
            minute <- read <$> count 2 digitChar
            void $ Text.Megaparsec.Char.char '.'
            second' <- read <$> count 2 digitChar
            sub <- optional subsecs

            let sec = daySegmToSeconds year day hour minute second'

            return $ makeTime sec (fromMaybe 0 sub) False

-- | subseconds parser
{-# INLINEABLE subsecs #-}
subsecs :: Parser Int32
subsecs = Text.Megaparsec.try $ do
    void $ Text.Megaparsec.Char.char '.'
    s <- read <$> count 3 digitChar
    return (s * 1000)

-- | Relative time conversion
{-# INLINEABLE daysSegmToSecondsRel #-}
daysSegmToSecondsRel ::
    Char -> Int -> Int -> Int -> Int -> Int -> (Int64, Bool)
daysSegmToSecondsRel sign year days hours minutes seconds =
    let s = sign == '-'
     in ( fromIntegral year
            * (3600 * 24 * 365)
            + fromIntegral days
            * 86400
            + fromIntegral hours
            * 3600
            + fromIntegral minutes
            * 60
            + fromIntegral seconds
        , s
        )

{-# INLINEABLE leapDays #-}
leapDays :: Int -> Int
leapDays year =
    let prevYear = year - 1
     in prevYear `div` 4 - prevYear `div` 100 + prevYear `div` 400

{-# INLINEABLE leapDays1970 #-}
leapDays1970 :: Int
leapDays1970 = leapDays 1970

-- | Copy of SCOS function to convert day segmented time into seconds
{-# INLINEABLE daySegmToSeconds #-}
daySegmToSeconds :: Int -> Int -> Int -> Int -> Int -> Int64
daySegmToSeconds year days hours minutes seconds =
    let leap_years = leapDays year - leapDays1970
        day_sec =
            if days /= 0
                then fromIntegral (days + leap_years - 1) * secsInDay
                else 0
        temp_sec = fromIntegral (year - 1970) * secsInYear
        temp_sec' =
            temp_sec
                + day_sec
                + fromIntegral hours
                * 3600
                + fromIntegral minutes
                * 60
                + fromIntegral seconds
     in temp_sec'

-- | Add two 'SunTime'
{-# INLINEABLE (<+>) #-}
(<+>) :: SunTime -> SunTime -> SunTime
t1 <+> t2 = addTimes t1 t2

-- | Subtract two 'SunTime'
{-# INLINEABLE (<->) #-}
(<->) :: SunTime -> SunTime -> SunTime
t1 <-> t2 = subTimes t1 t2

-- | Negate a 'SunTime'
{-# INLINEABLE negTime #-}
negTime :: SunTime -> SunTime
negTime (SunTime mic delta) = SunTime (-mic) delta

-- | add two sun times. Bails out with error if other times are used.
{-# INLINEABLE addTimes #-}
addTimes :: SunTime -> SunTime -> SunTime
addTimes t1 t2 =
    let mt1 = tdsTime t1
        mt2 = tdsTime t2
        bothDelta = isDelta t1 && isDelta t2
        summ = mt1 + mt2
     in microToTime summ bothDelta

{-# INLINEABLE addSpan #-}
addSpan :: SunTime -> TimeSpn a -> SunTime
addSpan (SunTime t d) spn = SunTime (t + getMicro spn) d

{-# INLINEABLE subSpan #-}
subSpan :: SunTime -> TimeSpn a -> SunTime
subSpan (SunTime t d) spn = SunTime (t - getMicro spn) d

-- | only implemented for sun time
{-# INLINEABLE subTimes #-}
subTimes :: SunTime -> SunTime -> SunTime
subTimes t1 t2 = addTimes t1 (negTime t2)

instance TimeRepConversion SunTime where
    {-# INLINEABLE timeToWord64 #-}
    timeToWord64 t = timeToWord64' (tdsSecs t) (tdsMicro t) (tdsDelta t)
    {-# INLINEABLE word64ToTime #-}
    word64ToTime val delta =
        let (sec, mic) = word64ToTime' val in makeTime sec mic delta

    {-# INLINEABLE timeToMicro #-}
    timeToMicro (SunTime usec _) = usec
    {-# INLINEABLE microToTime #-}
    microToTime = SunTime

{- | Class to indicate if a time value is a delta time. Can also be set
via the setDelta function
-}
class DeltaTime a where
    isDelta :: a -> Bool
    setDelta :: Bool -> a -> a

instance DeltaTime SunTime where
    {-# INLINEABLE isDelta #-}
    isDelta = tdsDelta
    {-# INLINEABLE setDelta #-}
    setDelta val t = t{tdsDelta = val}

-- | Data structure which contains the coefficients for correlation
data CorrelationCoefficients = CorrelationCoefficients
    { timCoeffGradient :: !Double
    , timCoeffOffset :: !Double
    }
    deriving (Show, Read)

-- | creates a coefficients data structure out of two real values (gradient and offset)
{-# INLINEABLE createCoeffs #-}
createCoeffs :: Double -> Double -> CorrelationCoefficients
createCoeffs = CorrelationCoefficients

-- | the default coefficients are gradient = 1.0, offset = 0
{-# INLINEABLE defaultCoeffs #-}
defaultCoeffs :: CorrelationCoefficients
defaultCoeffs = CorrelationCoefficients 1 0

newtype LeapSeconds = LeapSeconds {fromLeaps :: Int}
    deriving (Eq, Ord, Num, Real, Enum, Integral, Show, Read, Generic)

instance Serialise LeapSeconds
instance FromJSON LeapSeconds
instance ToJSON LeapSeconds where
    toEncoding = genericToEncoding defaultOptions

-- | correlation of the given ground time relative to a on-board time
{-# INLINEABLE mcsTimeToOBT #-}
mcsTimeToOBT :: SunTime -> CorrelationCoefficients -> SunTime
mcsTimeToOBT curTime coeff
    | isDelta curTime =
        curTime
    | otherwise =
        let tim = toDouble curTime
            obt' = tim * timCoeffGradient coeff + timCoeffOffset coeff
            obt = fromDouble obt' False
         in obt

{- | correlation of a given on-board time into
ground time
-}
{-# INLINEABLE obtToMcsTim #-}
obtToMcsTim :: SunTime -> CorrelationCoefficients -> SunTime
obtToMcsTim obt coeff
    | isDelta obt =
        obt
    | otherwise =
        let obt' = toDouble obt
            tim = (obt' - timCoeffOffset coeff) / timCoeffGradient coeff
            utt = fromDouble tim False
         in utt

{- | Perform leap second correction to a given time. This is for the
converstion from ground time to OBT
-}
{-# INLINEABLE uttObtLeap #-}
uttObtLeap :: LeapSeconds -> SunTime -> SunTime
uttObtLeap leaps (SunTime mic delta) =
    SunTime (mic + fromIntegral leaps * microSecInt) delta

{- | Perform leap second correction to a given time. This is for the
converstion from OBT to ground time
-}
{-# INLINEABLE obtUttLeap #-}
obtUttLeap :: LeapSeconds -> SunTime -> SunTime
obtUttLeap leaps (SunTime mic delta) =
    SunTime (mic - fromIntegral leaps * microSecInt) delta

-- | Epoch, which specifies the used mission epoch, including leap seconds
data Epoch = Epoch
    { epEpoch :: !Int64
    , epLeapSeconds :: !LeapSeconds
    }
    deriving (Show, Read, Eq)

instance Ord Epoch where
    compare (Epoch e1 _) (Epoch e2 _) = compare e1 e2

{- | constructor of an epoch. Takes the seconds for the epoch (in relation
to Unix time 1.1.1970) and the leap seconds. Both can be negative
(e.g. for TAI time, the epoch is negative since 1958 < 1970
-}
{-# INLINEABLE makeEpoch #-}
makeEpoch :: Int64 -> LeapSeconds -> Epoch
makeEpoch seconds leaps =
    Epoch (seconds * microSecInt) (leaps * fromIntegral microSecInt)

{- | Epoch time is encoded with a given epoch. The epoch is completely free,
some values have been specified as default (GPS, TAI and Year2000)
-}
data EpochTime = EpochTime
    { eptTime :: !Int64
    , eptDelta :: !Bool
    , eptEpoch :: !Epoch
    }
    deriving (Eq, Show, Read)

data FreeEpoch = FreeEpoch
    { epYear :: !Int
    , epDays :: !Int
    , epHours :: !Int
    , epMinutes :: !Int
    , epSeconds :: !Int
    }
    deriving (Eq, Ord, Show, Read, Generic)

instance Serialise FreeEpoch
instance FromJSON FreeEpoch
instance ToJSON FreeEpoch where
    toEncoding = genericToEncoding defaultOptions

-- | The Epoch type
data EpochType
    = UnixTime
    | GPSTime
    | TAITime
    | Year2000
    | Free FreeEpoch
    deriving (Eq, Ord, Show, Read, Generic)

instance Serialise EpochType
instance FromJSON EpochType
instance ToJSON EpochType where
    toEncoding = genericToEncoding defaultOptions

{-# INLINEABLE getEpoch #-}
getEpoch :: EpochType -> LeapSeconds -> Epoch
getEpoch UnixTime leaps = epochUnix leaps
getEpoch GPSTime leaps = epochGPS leaps
getEpoch TAITime leaps = epoch1958 leaps
getEpoch Year2000 leaps = epoch2000 leaps
getEpoch (Free FreeEpoch{..}) leaps =
    let secs =
            daySegmToSeconds
                epYear
                epDays
                epHours
                epMinutes
                epSeconds
        t = secs * 1_000_000
     in Epoch t leaps

-- | the null time for a time with an epoch
{-# INLINEABLE nullEpochTime #-}
nullEpochTime :: Epoch -> EpochTime
nullEpochTime = EpochTime 0 False

-- | the null time for GPS
{-# INLINEABLE nullGPSTime #-}
nullGPSTime :: LeapSeconds -> EpochTime
nullGPSTime leaps = EpochTime 0 False (epochGPS leaps)

-- | the null time for TAI
{-# INLINEABLE nullTAITime #-}
nullTAITime :: LeapSeconds -> EpochTime
nullTAITime leaps = EpochTime 0 False (epoch1958 leaps)

-- | the null time for a time with an epoch
{-# INLINEABLE nullEpochTimeRel #-}
nullEpochTimeRel :: Epoch -> EpochTime
nullEpochTimeRel = EpochTime 0 True

-- | the null time for GPS
{-# INLINEABLE nullGPSTimeRel #-}
nullGPSTimeRel :: LeapSeconds -> EpochTime
nullGPSTimeRel leaps = EpochTime 0 True (epochGPS leaps)

-- | the null time for TAI
{-# INLINEABLE nullTAITimeRel #-}
nullTAITimeRel :: LeapSeconds -> EpochTime
nullTAITimeRel leaps = EpochTime 0 True (epoch1958 leaps)

-- | one micro second in epoch time
{-# INLINEABLE oneMicroSecondEpoch #-}
oneMicroSecondEpoch :: Epoch -> EpochTime
oneMicroSecondEpoch = EpochTime 1 True

-- | one micro second GPS Time
{-# INLINEABLE oneMicroSecondGPS #-}
oneMicroSecondGPS :: LeapSeconds -> EpochTime
oneMicroSecondGPS leaps = EpochTime 1 True (epochGPS leaps)

-- | the epoch of TAI time (01.01.1958)
{-# INLINEABLE epoch1958 #-}
epoch1958 :: LeapSeconds -> Epoch
epoch1958 leaps =
    Epoch ((-378691200) * microSecInt) (leaps * fromIntegral microSecInt)

-- | the epoch of GPS time (06.01.1980)
{-# INLINEABLE epochGPS #-}
epochGPS :: LeapSeconds -> Epoch
epochGPS leaps =
    Epoch (315964800 * microSecInt) (leaps * fromIntegral microSecInt)

-- | the epoch of unix time (01.01.1970)
{-# INLINEABLE epochUnix #-}
epochUnix :: LeapSeconds -> Epoch
epochUnix leaps = Epoch 0 (leaps * fromIntegral microSecInt)

-- | the epoch of the year 2000
{-# INLINEABLE epoch2000 #-}
epoch2000 :: LeapSeconds -> Epoch
epoch2000 leaps =
    Epoch (946684800 * microSecInt) (leaps * fromIntegral microSecInt)

-- | the default epoch is the unix epoch
{-# INLINEABLE defaultEpoch #-}
defaultEpoch :: Epoch
defaultEpoch = epochUnix 0

instance DeltaTime EpochTime where
    {-# INLINEABLE isDelta #-}
    isDelta (EpochTime _ delta _) = delta
    {-# INLINEABLE setDelta #-}
    setDelta val (EpochTime mic _ ep) = EpochTime mic val ep

instance ToDouble EpochTime where
    {-# INLINEABLE toDouble #-}
    toDouble (EpochTime mic _ _) = fromIntegral mic / microSecond

-- {-# INLINABLE epochTimeToMicro #-}
-- epochTimeToMicro :: EpochTime -> Int64
-- epochTimeToMicro (EpochTime usec _ _) = usec
-- {-# INLINABLE microToEpochTime #-}
-- microToEpochTime :: Int64 -> Epoch -> Bool -> EpochTime
-- microToEpochTime val ep delta = EpochTime val delta ep

-- | Converts an 'EpochTime' to a 'SunTime'
{-# INLINEABLE epochTimeToSunTime #-}
epochTimeToSunTime :: EpochTime -> SunTime
epochTimeToSunTime (EpochTime m d (Epoch e l))
    | d = SunTime m d
    | otherwise = SunTime (m + e - fromIntegral l) d

-- | Converts a 'SunTime' with the given 'Epoch' into a 'EpochTime'
{-# INLINEABLE sunTimeToEpochTime #-}
sunTimeToEpochTime :: Epoch -> SunTime -> EpochTime
sunTimeToEpochTime ep@(Epoch e l) (SunTime mic delta) =
    let newmic
            | delta = mic
            | otherwise = (mic - e + fromIntegral l)
     in EpochTime newmic delta ep
