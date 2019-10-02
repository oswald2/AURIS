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
  , (<+>)
  , (<->)
  , addTimes
  , subTimes
  , negTime
  , LeapSeconds(..)
  , createCoeffs
  , defaultCoeffs
  , mcsTimeToOBT
  , obtToMcsTim
  , uttObtLeap
  , obtUttLeap
  )
where

import           RIO
import           RIO.Partial                    ( read )
import qualified RIO.Text                      as T

import           Control.Lens.Iso

import           Data.Bits
import           Data.Thyme.Clock
import           Data.Thyme.Time.Core
import           Data.Thyme.Clock.POSIX
import           Data.Thyme.Calendar.OrdinalDate

import           Formatting

import           Text.Megaparsec
--import           Text.Megaparsec.Char.Lexer
import           Text.Megaparsec.Char

import           General.Types                  ( ToDouble(..) )


type Parser = Parsec Void Text


{-# INLINEABLE microSecs #-}
microSecs :: Int64
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


-- | Time types
data SunTime = SunTime {
    tdsTime :: !Int64,
    tdsDelta :: !Bool
    }
    deriving (Eq, Show, Read)


-- | converts the time into a 'Double' represinting the seconds
-- from the Unix epoch. The fractional part are the subseconds
instance ToDouble SunTime where
  toDouble (SunTime msecs _) = fromIntegral msecs / 1_000_000

{-# INLINEABLE fromDouble #-}
fromDouble :: Double -> Bool -> SunTime
fromDouble secs = SunTime (round (secs * 1_000_000))

instance Ord SunTime where
  SunTime t1  False `compare` SunTime t2  False = t1 `compare` t2
  SunTime t1  True  `compare` SunTime t2  True  = t1 `compare` t2
  SunTime _t1 False `compare` SunTime _t2 True  = GT
  SunTime _t1 True  `compare` SunTime _t2 False = LT


{-# INLINEABLE makeTime #-}
makeTime :: Int64 -> Int32 -> Bool -> SunTime
makeTime sec usec delta =
  let (restsec, usec') = abs usec `quotRem` fromIntegral microSecInt
      sign             = if sec < 0 || usec < 0 then (-1) else 1
      newSec           = (abs sec + fromIntegral restsec)
      newMicro         = newSec * microSecInt + fromIntegral usec'
  in  SunTime (sign * newMicro) delta

{-# INLINEABLE tdsSecs #-}
tdsSecs :: SunTime -> Int64
tdsSecs (SunTime micro _) = micro `quot` microSecInt

{-# INLINEABLE tdsMicro #-}
tdsMicro :: SunTime -> Int32
tdsMicro (SunTime micro _) = fromIntegral (micro `rem` microSecInt)


-- | the null time for standard unix time
{-# INLINEABLE nullTime #-}
nullTime :: SunTime
nullTime = SunTime 0 False

{-# INLINEABLE nullRelTime #-}
nullRelTime :: SunTime
nullRelTime = SunTime 0 True


-- | one micro second in unix time
{-# INLINEABLE oneMicroSecond #-}
oneMicroSecond :: SunTime
oneMicroSecond = SunTime 1 True


-- | check if a time is a null time
{-# INLINABLE tdsNull #-}
tdsNull :: SunTime -> Bool
tdsNull (SunTime mic _) = mic == 0

{-# INLINABLE secsInDay #-}
secsInDay :: Int64
secsInDay = 86400

{-# INLINABLE milliSecsInDay #-}
milliSecsInDay :: Int64
milliSecsInDay = secsInDay * 1000

{-# INLINABLE microSecsInDay #-}
microSecsInDay :: Int64
microSecsInDay = secsInDay * 1_000_000

{-# INLINABLE microSecInt #-}
microSecInt :: Int64
microSecInt = 1_000_000

{-# INLINABLE secsInYear #-}
secsInYear :: Int64
secsInYear = 31536000

{-# INLINABLE microSecond #-}
microSecond :: Double
microSecond = fromIntegral microSecInt


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

{-# INLINABLE displayDouble #-}
displayDouble :: SunTime -> Text
displayDouble t = utf8BuilderToText $ displayShow (toDouble t) <> " s"



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
-- | format YYY.DDD.hh.mm.ss.mmmmmm
fromText :: Text -> Either Text SunTime
fromText str = case parse timeStr "" str of
  Left  err -> Left (T.pack (show err))
  Right x   -> Right x



-- | the time parser
timeStr :: Parser SunTime
timeStr = do
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
  sub    <- optional subsecs

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


{-# INLINABLE (<+>) #-}
(<+>) :: SunTime -> SunTime -> SunTime
t1 <+> t2 = addTimes t1 t2

{-# INLINABLE (<->) #-}
(<->) :: SunTime -> SunTime -> SunTime
t1 <-> t2 = subTimes t1 t2

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
        let (sec, mic) = word64ToTime' val in
        makeTime sec mic delta

    {-# INLINABLE timeToMicro #-}
    timeToMicro (SunTime usec _) = usec
    {-# INLINABLE microToTime #-}
    microToTime = SunTime 


class DeltaTime a where
    isDelta :: a -> Bool
    setDelta :: Bool -> a -> a

instance DeltaTime SunTime where
    {-# INLINABLE isDelta #-}
    isDelta = tdsDelta
    {-# INLINABLE setDelta #-}
    setDelta val t = t {tdsDelta = val}





-- | Data structure which contains the coefficients for correlation
data CorrCoefficients = CorrCoefficients {
    timCoeffGradient :: !Double,
    timCoeffOffset :: !Double
    } deriving (Show, Read)

-- | creates a coefficients data structure out of two real values (gradient and offset)
{-# INLINABLE createCoeffs #-}
createCoeffs :: Double -> Double -> CorrCoefficients
createCoeffs = CorrCoefficients 

-- | the default coefficients are gradient = 1.0, offset = 0
{-# INLINABLE defaultCoeffs #-}
defaultCoeffs :: CorrCoefficients
defaultCoeffs = CorrCoefficients 1 0


newtype LeapSeconds = LeapSeconds { unLeapSeconds :: Int }
    deriving (Eq, Ord, Num, Real, Enum, Integral, Show, Read)


-- | correlation of the given ground time relative to a start time.
-- | This function is only
{-# INLINABLE mcsTimeToOBT #-}
mcsTimeToOBT :: SunTime -> CorrCoefficients -> SunTime
mcsTimeToOBT curTime coeff
    | isDelta curTime
    = curTime
    | otherwise
    = let tim  = toDouble curTime
          obt' = tim * timCoeffGradient coeff + timCoeffOffset coeff
          obt  = fromDouble obt' False
      in  obt

-- | correlation of a given on-board time relative to a start time into
-- | ground time
{-# INLINABLE obtToMcsTim #-}
obtToMcsTim :: SunTime -> CorrCoefficients -> SunTime
obtToMcsTim obt coeff
    | isDelta obt
    = obt
    | otherwise
    = let obt' = toDouble obt
          tim  = (obt' - timCoeffOffset coeff) / timCoeffGradient coeff
          utt  = fromDouble tim False
      in  utt

-- | Perform leap second correction to a given time. This is for the
-- | converstion from ground time to OBT
{-# INLINABLE uttObtLeap #-}
uttObtLeap :: LeapSeconds -> SunTime -> SunTime
uttObtLeap leaps (SunTime mic delta) =
    SunTime (mic + fromIntegral leaps * microSecInt) delta



-- | Perform leap second correction to a given time. This is for the
-- | converstion from OBT to ground time
{-# INLINABLE obtUttLeap #-}
obtUttLeap :: LeapSeconds -> SunTime -> SunTime
obtUttLeap leaps (SunTime mic delta) =
    SunTime (mic - fromIntegral leaps * microSecInt) delta