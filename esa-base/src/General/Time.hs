{-|
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
{-# LANGUAGE OverloadedStrings
    , BangPatterns
    , GeneralizedNewtypeDeriving
    , DeriveGeneric
    , RecordWildCards
    , NumericUnderscores
    , FlexibleInstances
    , NoImplicitPrelude
#-}
module General.Time
  ( TimeRepConversion(..)
  , timeToWord64'
  , word64ToTime'
  , timeToMicro'
  , microToTime'
  , SunTime
  , makeTime
  , tdsSecs
  , tdsMicro
  , nullTime
  , nullRelTime
  , oneMicroSecond
  , tdsNull
  , secsInDay
  , secsInYear
  , milliSecsInDay
  , microSecsInDay
  , microSecInt
  , microSecond
  , displayRaw
  , displayISO
  , displayDouble
  , General.Time.getCurrentTime
  , fromDouble
  , DeltaTime(..)
  , edenTime
  , fromText
  , sunTimeParser
  , (<+>)
  , (<->)
  , addTimes
  , subTimes
  , negTime
  , LeapSeconds(..)
  , CorrelationCoefficients
  , createCoeffs
  , defaultCoeffs
  , mcsTimeToOBT
  , obtToMcsTim
  , uttObtLeap
  , obtUttLeap
  , makeEpoch
  , getEpoch
  , nullEpochTime
  , nullGPSTime
  , nullTAITime
  , nullEpochTimeRel
  , nullGPSTimeRel
  , nullTAITimeRel
  , oneMicroSecondEpoch
  , oneMicroSecondGPS
  , defaultEpoch
  , epoch1958
  , epochGPS
  , epochUnix
  , epoch2000
  , epochTimeToSunTime
  , sunTimeToEpochTime
  , Epoch
  , EpochType(..)
  , EpochTime(..)
  )
where

import           RIO
import           RIO.Partial                    ( read )
import qualified RIO.Text                      as T

import           Control.Lens.Iso
import           Codec.Serialise

import           Data.Bits
import           Data.Thyme.Clock
import           Data.Thyme.Time.Core
import           Data.Thyme.Clock.POSIX
import           Data.Thyme.Calendar.OrdinalDate
import           Data.Aeson

import           Formatting

import           Text.Megaparsec
--import           Text.Megaparsec.Char.Lexer
import           Text.Megaparsec.Char

import           General.Types                  ( ToDouble(..) )


type Parser = Parsec Void Text

-- | one micro second
{-# INLINABLE microSecs #-}
microSecs :: Int64
microSecs = 1_000_000


-- | This class is for handling time conversions of the different time types
-- This can be used in encoding as well as for displaying stuff, so
-- this is a bit more general than it looks like
class TimeRepConversion a where
    -- | converts time to a 64 bit word. High word are seconds, low word are
    -- | microseconds
    timeToWord64 :: a -> Word64
    word64ToTime :: Word64 -> Bool -> a

    -- | converts a time to microseconds in a 64 bit word since the epoch
    timeToMicro :: a -> Int64
    microToTime :: Int64 -> Bool -> a




{-# INLINABLE timeToWord64' #-}
timeToWord64' :: Int64 -> Int32 -> Bool -> Word64
timeToWord64' sec usec _ =
  let sec' :: Int64
      sec' = sec `shiftL` 32
  in  fromIntegral sec' .|. (fromIntegral usec .&. 0xFFFFFFFF)

{-# INLINABLE word64ToTime' #-}
word64ToTime' :: Word64 -> (Int64, Int32)
word64ToTime' val' =
  let val :: Int64
      val = fromIntegral val'
      sec = val `shiftR` 32
      usec :: Int32
      usec = fromIntegral (val .&. 0xFFFFFFFF)
  in  (sec, usec)


{-# INLINABLE timeToMicro' #-}
timeToMicro' :: Int64 -> Int32 -> Bool -> Int64
timeToMicro' sec usec _ =
  let sign = if sec < 0 || usec < 0 then (-1) else 1
  in  sign * (abs (sec) * microSecs + fromIntegral (abs usec))


{-# INLINABLE microToTime' #-}
microToTime' :: Int64 -> (Int64, Int32)
microToTime' x =
  let sign        = signum x
      absx        = abs x
      (sec, usec) = absx `quotRem` microSecs
  in  (sign * sec, fromIntegral (sign * usec))


-- | The basic sun data type. The sun time  is the general class on application
-- level which is used in the system. It is used in all user interface interactions
-- and is subject to time correlation and leap second correction (currently fixed leap
-- seconds taken from the configuration).
data SunTime = SunTime {
    tdsTime :: !Int64,
    tdsDelta :: !Bool
    }
    deriving (Eq, Show, Read, Generic)

instance NFData SunTime
instance Serialise SunTime
instance FromJSON SunTime
instance ToJSON SunTime where
    toEncoding = genericToEncoding defaultOptions


-- | converts the time into a 'Double' represinting the seconds
-- from the Unix epoch. The fractional part are the subseconds
instance ToDouble SunTime where
  toDouble (SunTime msecs _) = fromIntegral msecs / 1_000_000

-- | Convert from a double to a time. The double is assumed to
-- be in seconds
{-# INLINABLE fromDouble #-}
fromDouble :: Double -> Bool -> SunTime
fromDouble secs = SunTime (round (secs * 1_000_000))

instance Ord SunTime where
  SunTime t1  False `compare` SunTime t2  False = t1 `compare` t2
  SunTime t1  True  `compare` SunTime t2  True  = t1 `compare` t2
  SunTime _t1 False `compare` SunTime _t2 True  = GT
  SunTime _t1 True  `compare` SunTime _t2 False = LT

-- | Create a 'SunTime' out of seconds, microseconds and if it is a
-- delta time (True) or not
{-# INLINABLE makeTime #-}
makeTime :: Int64 -> Int32 -> Bool -> SunTime
makeTime sec usec delta =
  let (restsec, usec') = abs usec `quotRem` fromIntegral microSecInt
      sign             = if sec < 0 || usec < 0 then (-1) else 1
      newSec           = (abs sec + fromIntegral restsec)
      newMicro         = newSec * microSecInt + fromIntegral usec'
  in  SunTime (sign * newMicro) delta


-- | returns the seconds of the 'SunTime'
{-# INLINABLE tdsSecs #-}
tdsSecs :: SunTime -> Int64
tdsSecs (SunTime micro _) = micro `quot` microSecInt

-- | returns the micro seconds of the 'SunTime'
{-# INLINABLE tdsMicro #-}
tdsMicro :: SunTime -> Int32
tdsMicro (SunTime micro _) = fromIntegral (micro `rem` microSecInt)


-- | the null time for standard unix time
{-# INLINABLE nullTime #-}
nullTime :: SunTime
nullTime = SunTime 0 False

-- | a null relative time
{-# INLINABLE nullRelTime #-}
nullRelTime :: SunTime
nullRelTime = SunTime 0 True


-- | one micro second in unix time
{-# INLINABLE oneMicroSecond #-}
oneMicroSecond :: SunTime
oneMicroSecond = SunTime 1 True


-- | check if a time is a null time
{-# INLINABLE tdsNull #-}
tdsNull :: SunTime -> Bool
tdsNull (SunTime mic _) = mic == 0

-- | the seconds per day (no leaps)
{-# INLINABLE secsInDay #-}
secsInDay :: Int64
secsInDay = 86400

-- | the milli seconds in a day (no leaps)
{-# INLINABLE milliSecsInDay #-}
milliSecsInDay :: Int64
milliSecsInDay = secsInDay * 1000

-- | the micro seconds in a day (no leaps)
{-# INLINABLE microSecsInDay #-}
microSecsInDay :: Int64
microSecsInDay = secsInDay * 1_000_000

-- | the micro seconds per seoncd as 'Int64'
{-# INLINABLE microSecInt #-}
microSecInt :: Int64
microSecInt = 1_000_000

-- | the seconds in a year (no leaps)
{-# INLINABLE secsInYear #-}
secsInYear :: Int64
secsInYear = 31536000

-- | the micro seconds per second as a 'Double'
{-# INLINABLE microSecond #-}
microSecond :: Double
microSecond = fromIntegral microSecInt

-- | Returns the current system time as a 'SunTime'
{-# INLINABLE getCurrentTime #-}
getCurrentTime :: IO SunTime
getCurrentTime = do
  t <- Data.Thyme.Clock.getCurrentTime
  return (SunTime (t ^. posixTime . microseconds) False)


-- | show the low level internal data of a time
{-# INLINABLE displayRaw #-}
displayRaw :: SunTime -> Text
displayRaw (SunTime mic delta) =
  utf8BuilderToText ("SunTime " <> displayShow mic <> " " <> displayShow delta)

-- | Display a 'SunTime' in ISO format
{-# INLINABLE displayISO #-}
displayISO :: SunTime -> Text
displayISO (SunTime t False) =
  let t1 :: LocalTime
      t1            = t ^. from microseconds . from posixTime . utcLocalTime utc
      date          = localDay t1 ^. gregorian
      time          = localTimeOfDay t1
      (secs, micro) = fromEnum (todSec time) `quotRem` 1_000_000
  in  sformat
        ( (left 4 '0' %. int)
        % "-"
        % (left 2 '0' %. int)
        % "-"
        % (left 2 '0' %. int)
        % "T"
        % (left 2 '0' %. int)
        % "."
        % (left 2 '0' %. int)
        % "."
        % (left 2 '0' %. int)
        % "."
        % (left 6 '0' %. int)
        )
        (ymdYear date)
        (ymdMonth date)
        (ymdDay date)
        (todHour time)
        (todMin time)
        secs
        micro
displayISO (SunTime t True) =
  let t1 :: LocalTime
      t1            = t ^. from microseconds . from posixTime . utcLocalTime utc
      date          = localDay t1 ^. gregorian
      time          = localTimeOfDay t1
      (secs, micro) = fromEnum (todSec time) `quotRem` 1_000_000
      sign          = if t < 0 then '-' else '+'
  in  sformat
        ( Formatting.char
        % (left 4 '0' %. int)
        % "-"
        % (left 2 '0' %. int)
        % "-"
        % (left 2 '0' %. int)
        % "T"
        % (left 2 '0' %. int)
        % "."
        % (left 2 '0' %. int)
        % "."
        % (left 2 '0' %. shown)
        % "."
        % (left 6 '0' %. int)
        )
        sign
        (ymdYear date)
        (ymdMonth date)
        (ymdDay date)
        (todHour time)
        (todMin time)
        secs
        micro


instance Display SunTime where
  -- | display a 'SunTime' in SCOS format (with day of year)
  textDisplay (SunTime t False) =
    let t1 :: LocalTime
        t1 = t ^. from microseconds . from posixTime . utcLocalTime utc
        date          = localDay t1 ^. gregorian
        time          = localTimeOfDay t1
        dayOfYear     = odDay $ localDay t1 ^. ordinalDate
        (secs, micro) = fromEnum (todSec time) `quotRem` 1_000_000
    in  sformat
          ( (left 4 '0' %. int)
          % "."
          % (left 3 '0' %. int)
          % "."
          % (left 2 '0' %. int)
          % "."
          % (left 2 '0' %. int)
          % "."
          % (left 2 '0' %. int)
          % "."
          % (left 6 '0' %. int)
          )
          (ymdYear date)
          dayOfYear
          (todHour time)
          (todMin time)
          secs
          micro
  textDisplay tt =
    let secs  = sec `rem` 60
        mins  = sec `quot` 60 `rem` 60
        hours = sec `quot` 3600 `rem` 24
        days  = sec `quot` 86400 `rem` 365
        years = sec `quot` (86400 * 365)
        mic   = tdsMicro tt
        sec   = tdsSecs tt
        sign  = if sec < 0 then '-' else '+'
    in  sformat
          ( Formatting.char
          % (left 4 '0' %. int)
          % "."
          % (left 3 '0' %. int)
          % "."
          % (left 2 '0' %. int)
          % "."
          % (left 2 '0' %. int)
          % "."
          % (left 2 '0' %. int)
          % "."
          % (left 6 '0' %. int)
          )
          sign
          years
          days
          hours
          mins
          secs
          (abs mic)

-- | Display a SunTime as a floating point value (in seconds)
{-# INLINABLE displayDouble #-}
displayDouble :: SunTime -> Text
displayDouble t = utf8BuilderToText $ displayShow (toDouble t) <> " s"


-- | Convert a 'SunTime' into a binary 'ByteString' in the format for the EDEN
-- protocol
{-# INLINABLE edenTime #-}
edenTime :: SunTime -> ByteString
edenTime (SunTime t _) =
  let t1 :: LocalTime
      t1            = t ^. from microseconds . from posixTime . utcLocalTime utc
      date          = localDay t1 ^. gregorian
      time          = localTimeOfDay t1
      dayOfYear     = odDay $ localDay t1 ^. ordinalDate
      (secs, micro) = fromEnum (todSec time) `quotRem` 1_000_000
      str           = sformat
        ( (left 4 '0' %. int)
        % " "
        % (left 3 '0' %. int)
        % ":"
        % (left 2 '0' %. int)
        % ":"
        % (left 2 '0' %. int)
        % ":"
        % (left 2 '0' %. int)
        % "."
        % (left 3 '0' %. int)
        )
        (ymdYear date)
        dayOfYear
        (todHour time)
        (todMin time)
        secs
        (micro `quot` 1_000)
  in  encodeUtf8 str



-- | parses a 'Text' and returns a 'SunTime' time. The format is standard SCOS
-- format YYY.DDD.hh.mm.ss.mmmmmm
fromText :: Text -> Either Text SunTime
fromText str = case parse sunTimeParser "" str of
  Left  err -> Left (T.pack (show err))
  Right x   -> Right x



-- | the time parser
sunTimeParser :: Parser SunTime
sunTimeParser = do
  Text.Megaparsec.Char.space
  -- option 0 sign
  sign <- optional
    (choice [Text.Megaparsec.Char.char '-', Text.Megaparsec.Char.char '+'])
  year <- read <$> Text.Megaparsec.some digitChar
  void $ Text.Megaparsec.Char.char '.'
  day <- read <$> count 3 digitChar
  void $ Text.Megaparsec.Char.char '.'
  hour <- read <$> count 2 digitChar
  void $ Text.Megaparsec.Char.char '.'
  minute <- read <$> count 2 digitChar
  void $ Text.Megaparsec.Char.char '.'
  second' <- read <$> count 2 digitChar
  sub     <- optional subsecs

  let sec = daySegmToSeconds year day hour minute second'
      secRel sgn = daysSegmToSecondsRel sgn year day hour minute second'

  case sign of
    Just si -> do
      let (sec', neg) = secRel si
          time        = makeTime sec' (fromMaybe 0 sub) True
      if neg then return (negTime time) else return time
    Nothing -> return $ makeTime sec (fromMaybe 0 sub) False


-- | subseconds parser
{-# INLINABLE subsecs #-}
subsecs :: Parser Int32
subsecs = Text.Megaparsec.try $ do
  void $ Text.Megaparsec.Char.char '.'
  s <- read <$> count 3 digitChar
  return (s * 1000)


-- | Relative time conversion
{-# INLINABLE daysSegmToSecondsRel #-}
daysSegmToSecondsRel :: Char -> Int -> Int -> Int -> Int -> Int -> (Int64, Bool)
daysSegmToSecondsRel sign year days hours minutes seconds =
  let s = sign == '-'
  in  ( fromIntegral year
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

-- | Copy of SCOS function to convert day segmented time into seconds
{-# INLINABLE daySegmToSeconds #-}
daySegmToSeconds :: Int -> Int -> Int -> Int -> Int -> Int64
daySegmToSeconds year days hours minutes seconds =
  let {-year_leap = if ((year /= 0) && ((year `rem` 4) == 0) &&
                        (((year `rem` 100) /= 0) || (year `rem` 1000) == 0))
                    then 1
                    else 0-}
      go j leap_years'
        | j >= year
        = leap_years'
        | otherwise
        = if (j `rem` 4) == 0 && (((j `rem` 100) /= 0) || ((j `rem` 1000) == 0))
          then go (j + 1) (leap_years' + 1)
          else go (j + 1) leap_years'
      leap_years = go 1970 0
      day_sec    = if days /= 0
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
  in  temp_sec'

-- | Add two 'SunTime'
{-# INLINABLE (<+>) #-}
(<+>) :: SunTime -> SunTime -> SunTime
t1 <+> t2 = addTimes t1 t2

-- | Subtract two 'SunTime'
{-# INLINABLE (<->) #-}
(<->) :: SunTime -> SunTime -> SunTime
t1 <-> t2 = subTimes t1 t2

-- | Negate a 'SunTime'
{-# INLINABLE negTime #-}
negTime :: SunTime -> SunTime
negTime (SunTime mic delta) = SunTime (-mic) delta



-- | add two sun times. Bails out with error if other times are used.
{-# INLINABLE addTimes #-}
addTimes :: SunTime -> SunTime -> SunTime
addTimes t1 t2 =
  let mt1       = tdsTime t1
      mt2       = tdsTime t2
      bothDelta = isDelta t1 && isDelta t2
      summ      = mt1 + mt2
  in  microToTime summ bothDelta


-- | only implemented for sun time
{-# INLINABLE subTimes #-}
subTimes :: SunTime -> SunTime -> SunTime
subTimes t1 t2 = addTimes t1 (negTime t2)

instance TimeRepConversion SunTime where
  {-# INLINABLE timeToWord64 #-}
  timeToWord64 t = timeToWord64' (tdsSecs t) (tdsMicro t) (tdsDelta t)
  {-# INLINABLE word64ToTime #-}
  word64ToTime val delta =
    let (sec, mic) = word64ToTime' val in makeTime sec mic delta

  {-# INLINABLE timeToMicro #-}
  timeToMicro (SunTime usec _) = usec
  {-# INLINABLE microToTime #-}
  microToTime = SunTime

-- | Class to indicate if a time value is a delta time. Can also be set
-- via the setDelta function
class DeltaTime a where
    isDelta :: a -> Bool
    setDelta :: Bool -> a -> a

instance DeltaTime SunTime where
  {-# INLINABLE isDelta #-}
  isDelta = tdsDelta
  {-# INLINABLE setDelta #-}
  setDelta val t = t { tdsDelta = val }





-- | Data structure which contains the coefficients for correlation
data CorrelationCoefficients = CorrelationCoefficients {
    timCoeffGradient :: !Double,
    timCoeffOffset :: !Double
    } deriving (Show, Read)

-- | creates a coefficients data structure out of two real values (gradient and offset)
{-# INLINABLE createCoeffs #-}
createCoeffs :: Double -> Double -> CorrelationCoefficients
createCoeffs = CorrelationCoefficients

-- | the default coefficients are gradient = 1.0, offset = 0
{-# INLINABLE defaultCoeffs #-}
defaultCoeffs :: CorrelationCoefficients
defaultCoeffs = CorrelationCoefficients 1 0


newtype LeapSeconds = LeapSeconds { fromLeaps :: Int }
    deriving (Eq, Ord, Num, Real, Enum, Integral, Show, Read, Generic)

instance Serialise LeapSeconds
instance FromJSON LeapSeconds
instance ToJSON LeapSeconds where
    toEncoding = genericToEncoding defaultOptions


-- | correlation of the given ground time relative to a on-board time
{-# INLINABLE mcsTimeToOBT #-}
mcsTimeToOBT :: SunTime -> CorrelationCoefficients -> SunTime
mcsTimeToOBT curTime coeff
  | isDelta curTime
  = curTime
  | otherwise
  = let tim  = toDouble curTime
        obt' = tim * timCoeffGradient coeff + timCoeffOffset coeff
        obt  = fromDouble obt' False
    in  obt

-- | correlation of a given on-board time into
-- ground time
{-# INLINABLE obtToMcsTim #-}
obtToMcsTim :: SunTime -> CorrelationCoefficients -> SunTime
obtToMcsTim obt coeff
  | isDelta obt
  = obt
  | otherwise
  = let obt' = toDouble obt
        tim  = (obt' - timCoeffOffset coeff) / timCoeffGradient coeff
        utt  = fromDouble tim False
    in  utt

-- | Perform leap second correction to a given time. This is for the
-- converstion from ground time to OBT
{-# INLINABLE uttObtLeap #-}
uttObtLeap :: LeapSeconds -> SunTime -> SunTime
uttObtLeap leaps (SunTime mic delta) =
  SunTime (mic + fromIntegral leaps * microSecInt) delta



-- | Perform leap second correction to a given time. This is for the
-- converstion from OBT to ground time
{-# INLINABLE obtUttLeap #-}
obtUttLeap :: LeapSeconds -> SunTime -> SunTime
obtUttLeap leaps (SunTime mic delta) =
  SunTime (mic - fromIntegral leaps * microSecInt) delta





-- | Epoch, which specifies the used mission epoch, including leap seconds
data Epoch = Epoch {
    epEpoch :: !Int64,
    epLeapSeconds :: !LeapSeconds
    }
    deriving (Show, Read, Eq)

instance Ord Epoch where
  compare (Epoch e1 _) (Epoch e2 _) = compare e1 e2


-- | constructor of an epoch. Takes the seconds for the epoch (in relation
-- to Unix time 1.1.1970) and the leap seconds. Both can be negative
-- (e.g. for TAI time, the epoch is negative since 1958 < 1970
{-# INLINABLE makeEpoch #-}
makeEpoch :: Int64 -> LeapSeconds -> Epoch
makeEpoch seconds leaps =
  Epoch (seconds * microSecInt) (leaps * fromIntegral microSecInt)


-- | Epoch time is encoded with a given epoch. The epoch is completely free,
-- some values have been specified as default (GPS, TAI and Year2000)
data EpochTime = EpochTime {
    eptTime :: !Int64,
    eptDelta :: !Bool,
    eptEpoch :: !Epoch
    }
    deriving (Eq, Show, Read)

-- | The Epoch type
data EpochType =
    UnixTime
    | GPSTime
    | TAITime
    | Year2000
    deriving (Eq, Ord, Enum, Show, Read, Generic)

instance Serialise EpochType
instance FromJSON EpochType
instance ToJSON EpochType where
  toEncoding = genericToEncoding defaultOptions

{-# INLINABLE getEpoch #-}
getEpoch :: EpochType -> LeapSeconds -> Epoch
getEpoch UnixTime leaps = epochUnix leaps
getEpoch GPSTime  leaps = epochGPS leaps
getEpoch TAITime  leaps = epoch1958 leaps
getEpoch Year2000 leaps = epoch2000 leaps

-- | the null time for a time with an epoch
{-# INLINABLE nullEpochTime #-}
nullEpochTime :: Epoch -> EpochTime
nullEpochTime = EpochTime 0 False


-- | the null time for GPS
{-# INLINABLE nullGPSTime #-}
nullGPSTime :: LeapSeconds -> EpochTime
nullGPSTime leaps = EpochTime 0 False (epochGPS leaps)
-- | the null time for TAI
{-# INLINABLE nullTAITime #-}
nullTAITime :: LeapSeconds -> EpochTime
nullTAITime leaps = EpochTime 0 False (epoch1958 leaps)


-- | the null time for a time with an epoch
{-# INLINABLE nullEpochTimeRel #-}
nullEpochTimeRel :: Epoch -> EpochTime
nullEpochTimeRel = EpochTime 0 True


-- | the null time for GPS
{-# INLINABLE nullGPSTimeRel #-}
nullGPSTimeRel :: LeapSeconds -> EpochTime
nullGPSTimeRel leaps = EpochTime 0 True (epochGPS leaps)
-- | the null time for TAI
{-# INLINABLE nullTAITimeRel #-}
nullTAITimeRel :: LeapSeconds -> EpochTime
nullTAITimeRel leaps = EpochTime 0 True (epoch1958 leaps)

-- | one micro second in epoch time
{-# INLINABLE oneMicroSecondEpoch #-}
oneMicroSecondEpoch :: Epoch -> EpochTime
oneMicroSecondEpoch = EpochTime 1 True

-- | one micro second GPS Time
{-# INLINABLE oneMicroSecondGPS #-}
oneMicroSecondGPS :: LeapSeconds -> EpochTime
oneMicroSecondGPS leaps = EpochTime 1 True (epochGPS leaps)


-- | the epoch of TAI time (01.01.1958)
{-# INLINABLE epoch1958 #-}
epoch1958 :: LeapSeconds -> Epoch
epoch1958 leaps =
  Epoch ((-378691200) * microSecInt) (leaps * fromIntegral microSecInt)

-- | the epoch of GPS time (06.01.1980)
{-# INLINABLE epochGPS #-}
epochGPS :: LeapSeconds -> Epoch
epochGPS leaps =
  Epoch (315964800 * microSecInt) (leaps * fromIntegral microSecInt)

  -- | the epoch of unix time (01.01.1970)
{-# INLINABLE epochUnix #-}
epochUnix :: LeapSeconds -> Epoch
epochUnix leaps = Epoch 0 (leaps * fromIntegral microSecInt)

-- | the epoch of the year 2000
{-# INLINABLE epoch2000 #-}
epoch2000 :: LeapSeconds -> Epoch
epoch2000 leaps =
  Epoch (946684800 * microSecInt) (leaps * fromIntegral microSecInt)

-- | the default epoch is the unix epoch
{-# INLINABLE defaultEpoch #-}
defaultEpoch :: Epoch
defaultEpoch = epochUnix 0


instance DeltaTime EpochTime where
  {-# INLINABLE isDelta #-}
  isDelta (EpochTime _ delta _) = delta
  {-# INLINABLE setDelta #-}
  setDelta val (EpochTime mic _ ep) = EpochTime mic val ep


instance ToDouble EpochTime where
  {-# INLINABLE toDouble #-}
  toDouble (EpochTime mic _ _) = fromIntegral mic / microSecond


-- {-# INLINABLE epochTimeToMicro #-}
-- epochTimeToMicro :: EpochTime -> Int64
-- epochTimeToMicro (EpochTime usec _ _) = usec
-- {-# INLINABLE microToEpochTime #-}
-- microToEpochTime :: Int64 -> Epoch -> Bool -> EpochTime
-- microToEpochTime val ep delta = EpochTime val delta ep


-- | Converts an 'EpochTime' to a 'SunTime'
{-# INLINABLE epochTimeToSunTime #-}
epochTimeToSunTime :: EpochTime -> SunTime
epochTimeToSunTime (EpochTime m d (Epoch e l))
  | d         = SunTime m d
  | otherwise = SunTime (m + e - fromIntegral l) d


-- | Converts a 'SunTime' with the given 'Epoch' into a 'EpochTime'
{-# INLINABLE sunTimeToEpochTime #-}
sunTimeToEpochTime :: Epoch -> SunTime -> EpochTime
sunTimeToEpochTime ep@(Epoch e l) (SunTime mic delta) =
  let newmic | delta     = mic
             | otherwise = (mic - e + fromIntegral l)
  in  EpochTime newmic delta ep
