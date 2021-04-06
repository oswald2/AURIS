module Data.PUS.EncTime
    ( CUCTime(..)
    , CDSTime(..)
    , CucEncoding(..)
    , mkCUCTime
    , mkCUC
    , mkCUC_1
    , mkCUC_2
    , mkCUC_3
    , mkCUCUnix
    , cucSetEncoding
    , cucGetEncoding
    , mkCDSTime
    , nullCUCTime
    , nullCDSTime
    , nullCUCTimeRel
    , withinEps
    , epsilon
    , oneMicroSecondCUC
    , oneMicroSecondCDS
    , microSecsInDay
    , cucTimeBuilder
    , cdsTimeBuilder
    , cucTimeParser
    , cdsTimeParser
    , cucEncodingSize
    , cucTimeIsDelta
    , cucTimeSetDelta
    , cucTimeToEpochTime
    , epochTimeToCUCTime
    , sunTimeToCUCTime
    , cucTimeToSunTime
    , cdsTimeToEpochTime
    , epochTimeToCDSTime
    , sunTimeToCDSTime
    , cdsTimeToSunTime
    , getValueCucTime
    , microToCUC
    , sizeof
    , ptcPfcEncoding
    ) where

import           RIO                     hiding ( Builder )
--import qualified RIO.Text                      as T
import           ByteString.StrictBuilder
import           Control.Lens                   ( from )

import           Data.Binary
import           Data.Aeson
import           Data.Bits                      ( Bits
                                                    ( (.|.)
                                                    , complement
                                                    , shiftR
                                                    , (.&.)
                                                    , shiftL
                                                    )
                                                )
import           Data.Attoparsec.ByteString     ( Parser )
import qualified Data.Attoparsec.ByteString    as A
import qualified Data.Attoparsec.Binary        as A
import           Data.Word.Word24

import           Codec.Serialise

import           General.SizeOf

import           General.Time
import           General.SetBitField            ( SetValue(..) )
import           General.GetBitField            ( GetValue(..) )
import           General.Types                  ( ByteOffset
                                                , Endian(..)
                                                )
import           General.PUSTypes               ( PFC(..)
                                                , PTC(..)
                                                )
import qualified Text.Builder                  as TB

import           Data.Thyme.Time.Core           ( YearMonthDay(ymdYear)
                                                , LocalTime
                                                    ( localTimeOfDay
                                                    , localDay
                                                    )
                                                , TimeOfDay
                                                    ( todSec
                                                    , todHour
                                                    , todMin
                                                    )
                                                , utc
                                                , utcLocalTime
                                                , gregorian
                                                , TimeDiff(microseconds)
                                                )
import           Data.Thyme.Clock.POSIX         ( posixTime )
import           Data.Thyme.Calendar.OrdinalDate
                                                ( OrdinalDate(odDay)
                                                , ordinalDate
                                                )



data CucEncoding =
    Cuc4
    | Cuc41
    | Cuc42
    | Cuc43
    | CucUnix
    deriving(Eq, Ord, Enum, Show, Read, Generic)

instance NFData CucEncoding
instance Binary CucEncoding
instance Serialise CucEncoding
instance FromJSON CucEncoding
instance ToJSON CucEncoding where
    toEncoding = genericToEncoding defaultOptions

cucEncodingSize :: CucEncoding -> Int
cucEncodingSize Cuc4    = 4
cucEncodingSize Cuc41   = 5
cucEncodingSize Cuc42   = 6
cucEncodingSize Cuc43   = 7
cucEncodingSize CucUnix = 8


ptcPfcEncoding :: PTC -> PFC -> Maybe CucEncoding
ptcPfcEncoding (PTC 9 ) (PFC 15) = Just Cuc4
ptcPfcEncoding (PTC 9 ) (PFC 16) = Just Cuc41
ptcPfcEncoding (PTC 9 ) (PFC 17) = Just Cuc42
ptcPfcEncoding (PTC 9 ) (PFC 18) = Just Cuc43
ptcPfcEncoding (PTC 10) (PFC 15) = Just Cuc4
ptcPfcEncoding (PTC 10) (PFC 16) = Just Cuc41
ptcPfcEncoding (PTC 10) (PFC 17) = Just Cuc42
ptcPfcEncoding (PTC 10) (PFC 18) = Just Cuc43
ptcPfcEncoding _        _        = Nothing


-- | Time types. CUC Time is standard unix time with normal encoding of
-- | 4 bytes coards and 2 bytes fine time
data CUCTime = CUCTime !CucEncoding !Int64 !Word32 !Bool
    deriving (Eq, Show, Read, Generic)

instance NFData CUCTime
instance Binary CUCTime
instance Serialise CUCTime
instance FromJSON CUCTime
instance ToJSON CUCTime where
    toEncoding = genericToEncoding defaultOptions

instance Ord CUCTime where
    compare (CUCTime _ s1 m1 d1) (CUCTime _ s2 m2 d2)
        | d1 == d2 = case compare s1 s2 of
            LT -> LT
            GT -> GT
            EQ -> compare m1 m2
        | d1 && not d2 = LT
        | otherwise = GT

-- | Time is encoded in CDS Time format.
data CDSTime = CDSTime !Word16 !Word32 (Maybe Word16)
    deriving (Eq, Show, Read, Generic)

instance NFData CDSTime
instance Binary CDSTime
instance Serialise CDSTime
instance FromJSON CDSTime
instance ToJSON CDSTime where
    toEncoding = genericToEncoding defaultOptions


instance SizeOf CUCTime where
    sizeof (CUCTime Cuc4    _ _ _) = cucEncodingSize Cuc4
    sizeof (CUCTime Cuc41   _ _ _) = cucEncodingSize Cuc41
    sizeof (CUCTime Cuc42   _ _ _) = cucEncodingSize Cuc42
    sizeof (CUCTime Cuc43   _ _ _) = cucEncodingSize Cuc43
    sizeof (CUCTime CucUnix _ _ _) = cucEncodingSize CucUnix

instance SizeOf CDSTime where
    sizeof (CDSTime _ _ (Just _)) = 8
    sizeof (CDSTime _ _ Nothing ) = 6

instance Display CUCTime where
    textDisplay t'@(CUCTime _ _ _ False) =
        let t = timeToMicro t'
            t1 :: LocalTime
            t1 = t ^. from microseconds . from posixTime . utcLocalTime utc
            date          = localDay t1 ^. gregorian
            time          = localTimeOfDay t1
            dayOfYear     = odDay $ localDay t1 ^. ordinalDate
            (secs, micro) = fromEnum (todSec time) `quotRem` 1_000_000
        in  TB.run
                $  TB.padFromLeft 4 '0' (TB.decimal (ymdYear date))
                <> TB.char '.'
                <> TB.padFromLeft 3 '0' (TB.decimal dayOfYear)
                <> TB.char '.'
                <> TB.padFromLeft 2 '0' (TB.decimal (todHour time))
                <> TB.char '.'
                <> TB.padFromLeft 2 '0' (TB.decimal (todMin time))
                <> TB.char '.'
                <> TB.padFromLeft 2 '0' (TB.decimal secs)
                <> TB.char '.'
                <> TB.padFromLeft 6 '0' (TB.decimal micro)
    textDisplay (CUCTime _ sec mic True) =
        let secs  = sec `rem` 60
            mins  = sec `quot` 60 `rem` 60
            hours = sec `quot` 3600 `rem` 24
            days  = sec `quot` 86400 `rem` 365
            years = sec `quot` (86400 * 365)
            sign  = if sec < 0 then '-' else '+'
        in  TB.run
                $  TB.char sign
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


{-# INLINABLE mkCUCTime #-}
mkCUCTime :: CucEncoding -> Int64 -> Int32 -> Bool -> CUCTime
mkCUCTime enc sec usec delta =
    let (restsec, usec') = abs usec `quotRem` fromIntegral microSecInt
        sign             = if sec < 0 || usec < 0 then (-1) else 1
        newSec           = (abs sec + fromIntegral restsec)
        newMicro         = usec'
    in  CUCTime enc (sign * newSec) (fromIntegral newMicro) delta

mkCUCUnix :: Int64 -> Int32 -> Bool -> CUCTime
mkCUCUnix sec usec delta =
    let (restsec, usec') = abs usec `quotRem` fromIntegral microSecInt
        sign             = if sec < 0 || usec < 0 then (-1) else 1
        newSec           = (abs sec + fromIntegral restsec)
        newMicro         = usec'
    in  CUCTime CucUnix (sign * newSec) (fromIntegral newMicro) delta

mkCUC :: Integral a => a -> Bool -> CUCTime
mkCUC s d = CUCTime Cuc4 (fromIntegral s) 0 d

mkCUC_1 :: Integral a => a -> Word8 -> Bool -> CUCTime
mkCUC_1 s m d = CUCTime
    Cuc41
    (fromIntegral s)
    (round ((fromIntegral m :: Double) * 1_000_000 / 256))
    d

mkCUC_2 :: Integral a => a -> Word16 -> Bool -> CUCTime
mkCUC_2 s m d = CUCTime
    Cuc42
    (fromIntegral s)
    (round ((fromIntegral m :: Double) * 1_000_000 / 65536))
    d

mkCUC_3 :: Integral a => a -> Word24 -> Bool -> CUCTime
mkCUC_3 s m d = CUCTime
    Cuc43
    (fromIntegral s)
    (round ((fromIntegral m :: Double) * 1_000_000 / (2 ** 24)))
    d

cucSetEncoding :: CUCTime -> CucEncoding -> CUCTime
cucSetEncoding (CUCTime _ s m d) enc = CUCTime enc s m d

cucGetEncoding :: CUCTime -> CucEncoding
cucGetEncoding (CUCTime enc _ _ _) = enc

{-# INLINABLE cucTimeIsDelta #-}
cucTimeIsDelta :: CUCTime -> Bool
cucTimeIsDelta (CUCTime _ _ _ x) = x

{-# INLINABLE cucTimeSetDelta #-}
cucTimeSetDelta :: Bool -> CUCTime -> CUCTime
cucTimeSetDelta delta (CUCTime enc s m _) = CUCTime enc s m delta


{-# INLINABLE mkCDSTime #-}
mkCDSTime :: Word16 -> Word32 -> Maybe Word16 -> CDSTime
mkCDSTime days milli (Just micro) =
    let (ov1, !micro') = micro `quotRem` 1000
        (ov2, milli') =
            (milli + fromIntegral ov1) `quotRem` fromIntegral milliSecsInDay
    in  CDSTime (days + fromIntegral ov2) milli' (Just micro')
mkCDSTime days milli Nothing =
    let (ov2, milli') = milli `quotRem` fromIntegral milliSecsInDay
    in  CDSTime (days + fromIntegral ov2) milli' Nothing


epsilon :: Double
epsilon = 25.5

-- | compare the times withtin a epsilon. This is sometimes necessary
-- | as the subseconds are microseconds, but the encoding ist only
-- | 16 bits (so only 65536 values), which is a loss of information
-- | When encoding -> decoding, the timestamp is therefore not exact
{-# INLINABLE withinEps #-}
withinEps :: Double -> CUCTime -> CUCTime -> Bool
withinEps eps (CUCTime _ secs1 micro1 delta1) (CUCTime _ secs2 micro2 delta2) =
    secs1
        == secs2
        && fromIntegral (abs (micro1 - micro2))
        <  eps
        && delta1
        == delta2


-- | the null time for standard unix time
{-# INLINABLE nullCUCTime #-}
nullCUCTime :: CucEncoding -> CUCTime
nullCUCTime enc = CUCTime enc 0 0 False



-- | the null time for a CDS time
{-# INLINABLE nullCDSTime #-}
nullCDSTime :: CDSTime
nullCDSTime = CDSTime 0 0 (Just 0)


-- | the null time for standard unix time
{-# INLINABLE nullCUCTimeRel #-}
nullCUCTimeRel :: CucEncoding -> CUCTime
nullCUCTimeRel enc = CUCTime enc 0 0 True


-- | one micro second in unix time
{-# INLINABLE oneMicroSecondCUC #-}
oneMicroSecondCUC :: CucEncoding -> CUCTime
oneMicroSecondCUC enc = CUCTime enc 0 1 True
-- | one micro second in CDS time
{-# INLINABLE oneMicroSecondCDS #-}
oneMicroSecondCDS :: CDSTime
oneMicroSecondCDS = CDSTime 0 0 (Just 1)





{-# INLINABLE cucTimeBuilder #-}
cucTimeBuilder :: CUCTime -> Builder
cucTimeBuilder (CUCTime Cuc4 sec _mic _) =
    let s = toEncodedSec sec in word32BE s
cucTimeBuilder (CUCTime Cuc41 sec mic _) =
    let (s, m) = toEncoded41 sec mic in word32BE s <> word8 m
cucTimeBuilder (CUCTime Cuc42 sec mic _) =
    let (s, m) = toEncoded42 sec mic in word32BE s <> word16BE m
cucTimeBuilder (CUCTime Cuc43 sec mic _) =
    let (s, m) = toEncoded43 sec mic
        !m1    = fromIntegral $ (m .&. 0xFF0000) `shiftR` 16
        !m2    = fromIntegral $ (m .&. 0xFF00) `shiftR` 8
        !m3    = fromIntegral $ m .&. 0xFF0000
    in  word32BE s <> word8 m1 <> word8 m2 <> word8 m3
cucTimeBuilder (CUCTime CucUnix sec mic _) =
    let s = toEncodedSec sec in word32BE s <> word32BE mic


instance SetValue CUCTime where
    {-# INLINABLE setValue #-}
    setValue vec off endian (CUCTime Cuc42 sec mic _) = do
        let (s, m) = toEncoded42 sec mic
        setValue vec off       endian s
        setValue vec (off + 4) endian m
    setValue vec off endian (CUCTime Cuc4 sec _ _) = do
        let s = toEncodedSec sec
        setValue vec off endian s
    setValue vec off endian (CUCTime Cuc41 sec mic _) = do
        let (s, m) = toEncoded41 sec mic
        setValue vec off       endian s
        setValue vec (off + 4) endian m
    setValue vec off endian (CUCTime Cuc43 sec mic _) = do
        let (s, m) = toEncoded43 sec mic
        setValue vec off       endian s
        setValue vec (off + 4) endian m
    setValue vec off endian (CUCTime CucUnix sec mic _) = do
        let s = toEncodedSec sec
        setValue vec off endian s
        setValue vec (off + 4) endian mic

{-# INLINABLE getValueCucTime #-}
getValueCucTime
    :: ByteString -> ByteOffset -> Endian -> CucEncoding -> Maybe CUCTime
getValueCucTime byts off bo Cuc4 =
    let se = getValue byts off bo
    in  case se of
            Just se' -> Just $ cucTimeFromBinary4 se' False
            _        -> Nothing
getValueCucTime byts off BiE Cuc41 =
    let se = getValue byts off BiE
        m  = getValue byts (off + 4) BiE
    in  case (se, m) of
            (Just se', Just m') -> Just $ cucTimeFromBinary41 se' m' False
            _                   -> Nothing
getValueCucTime byts off BiE Cuc42 =
    let se = getValue byts off BiE
        m  = getValue byts (off + 4) BiE
    in  case (se, m) of
            (Just se', Just m') -> Just $ cucTimeFromBinary42 se' m' False
            _                   -> Nothing
getValueCucTime byts off BiE Cuc43 =
    let se = getValue byts off BiE
        m  = getValue byts (off + 4) BiE
    in  case (se, m) of
            (Just se', Just m') -> Just $ cucTimeFromBinary43 se' m' False
            _                   -> Nothing
getValueCucTime byts off BiE CucUnix =
    let se = getValue byts off BiE
        m  = getValue byts (off + 4) BiE
    in  case (se, m) of
            (Just se', Just m') -> Just $ cucTimeFromBinary44 se' m' False
            _                   -> Nothing


getValueCucTime byts off LiE Cuc41 =
    let m  = getValue byts off LiE
        se = getValue byts (off + 1) LiE
    in  case (se, m) of
            (Just se', Just m') -> Just $ cucTimeFromBinary41 se' m' False
            _                   -> Nothing
getValueCucTime byts off LiE Cuc42 =
    let m  = getValue byts off LiE
        se = getValue byts (off + 2) LiE
    in  case (se, m) of
            (Just se', Just m') -> Just $ cucTimeFromBinary42 se' m' False
            _                   -> Nothing
getValueCucTime byts off LiE Cuc43 =
    let m  = getValue byts off LiE
        se = getValue byts (off + 3) LiE
    in  case (se, m) of
            (Just se', Just m') -> Just $ cucTimeFromBinary43 se' m' False
            _                   -> Nothing
getValueCucTime byts off LiE CucUnix =
    let m  = getValue byts off LiE
        se = getValue byts (off + 4) LiE
    in  case (se, m) of
            (Just se', Just m') -> Just $ cucTimeFromBinary44 se' m' False
            _                   -> Nothing




{-# INLINABLE cdsTimeBuilder #-}
cdsTimeBuilder :: CDSTime -> Builder
cdsTimeBuilder (CDSTime days milli micro) =
    word16BE days <> word32BE milli <> case micro of
        Just m  -> word16BE m
        Nothing -> mempty


{-# INLINABLE cdsTimeParser #-}
cdsTimeParser :: Parser CDSTime
cdsTimeParser = do
    days  <- A.anyWord16be
    milli <- A.anyWord32be
    micro <- A.anyWord16be
    pure $ CDSTime days milli (Just micro)


{-# INLINABLE toEncodedSec #-}
toEncodedSec :: Int64 -> Word32
toEncodedSec sec =
    let sign :: Int
        !sign = if sec < 0 then (-1) else 1
        sec' :: Word32
        !sec' = fromIntegral (abs sec)
        !val' = if sign < 0 then complement sec' + 1 else sec'
    in  val'

{-# INLINABLE toEncoded42 #-}
toEncoded42 :: Int64 -> Word32 -> (Word32, Word16)
toEncoded42 sec mic =
    let sign :: Int
        !sign = if sec < 0 || mic < 0 then (-1) else 1
        absm :: Double
        !absm = fromIntegral (abs mic)
        sec' :: Word32
        !sec' = fromIntegral (abs sec)
        mic' :: Word16
        !mic' = round (absm / 1000000.0 * 65536.0)
        val :: Word64
        !val       = fromIntegral sec' `shiftL` 16 .|. fromIntegral mic'
        !val'      = if sign < 0 then complement val + 1 else val
        !resultSec = fromIntegral ((val' `shiftR` 16) .&. 0xFFFFFFFF)
        !resultMic = fromIntegral (val' .&. 0xFFFF)
    in  (resultSec, resultMic)


{-# INLINABLE toEncoded41 #-}
toEncoded41 :: Int64 -> Word32 -> (Word32, Word8)
toEncoded41 sec mic =
    let sign :: Int
        !sign = if sec < 0 || mic < 0 then (-1) else 1
        secs  = toEncodedSec sec
        absm :: Double
        !absm = fromIntegral (abs mic)
        mic' :: Word8
        !mic' = round (absm / 1000000.0 * 256.0)
        val :: Word64
        !val       = fromIntegral secs `shiftL` 8 .|. fromIntegral mic'
        !val'      = if sign < 0 then complement val + 1 else val
        !resultSec = fromIntegral ((val' `shiftR` 8) .&. 0xFF_FF_FF_FF)
        !resultMic = fromIntegral (val' .&. 0xFF)
    in  (resultSec, resultMic)

{-# INLINABLE toEncoded43 #-}
toEncoded43 :: Int64 -> Word32 -> (Word32, Word24)
toEncoded43 sec mic =
    let sign :: Int
        !sign = if sec < 0 || mic < 0 then (-1) else 1
        secs  = toEncodedSec sec
        absm :: Double
        !absm = fromIntegral (abs mic)
        mic' :: Word24
        !mic' = round (absm / 1000000.0 * 16777216.0)
        val :: Word64
        !val       = fromIntegral secs `shiftL` 24 .|. fromIntegral mic'
        !val'      = if sign < 0 then complement val + 1 else val
        !resultSec = fromIntegral ((val' `shiftR` 24) .&. 0xFF_FF_FF_FF)
        !resultMic = fromIntegral (val' .&. 0xFF_FF_FF)
    in  (resultSec, resultMic)


{-# INLINABLE cucTimeParser #-}
cucTimeParser :: CUCTime -> Parser CUCTime
cucTimeParser (CUCTime Cuc42 _ _ delta) = do
    se <- A.anyWord32be
    m  <- A.anyWord16be
    pure (cucTimeFromBinary42 se m delta)
cucTimeParser (CUCTime Cuc4 _ _ delta) = do
    se <- A.anyWord32be
    pure $ CUCTime Cuc4 (fromIntegral se) 0 delta
cucTimeParser (CUCTime Cuc41 _ _ delta) = do
    se <- A.anyWord32be
    m  <- A.anyWord8
    pure $ cucTimeFromBinary41 se m delta
cucTimeParser (CUCTime Cuc43 _ _ delta) = do
    se <- A.anyWord32be
    m1 <- A.anyWord8
    m2 <- A.anyWord8
    m3 <- A.anyWord8
    let m :: Word24
        !m =
            ((fromIntegral m1) `shiftL` 16)
                .|. ((fromIntegral m2) `shiftL` 8)
                .|. (fromIntegral m3)
    pure $ cucTimeFromBinary43 se m delta
cucTimeParser (CUCTime CucUnix _ _ delta) = do
    se <- A.anyWord32be
    m  <- A.anyWord32be
    pure $ cucTimeFromBinary44 se m delta



{-# INLINABLE cucTimeFromBinary4 #-}
cucTimeFromBinary4 :: Word32 -> Bool -> CUCTime
cucTimeFromBinary4 se delta =
    let val1 :: Int32
        !val1 = fromIntegral se
    in  CUCTime Cuc41 (fromIntegral val1) 0 delta

{-# INLINABLE cucTimeFromBinary41 #-}
cucTimeFromBinary41 :: Word32 -> Word8 -> Bool -> CUCTime
cucTimeFromBinary41 se m delta =
    let val1 :: Int32
        !val1 = fromIntegral se
        micro :: Double
        micro  = fromIntegral m
        micro' = round (micro / 256.0 * 1000000.0)
    in  CUCTime Cuc41 (fromIntegral val1) micro' delta

{-# INLINABLE cucTimeFromBinary42 #-}
cucTimeFromBinary42 :: Word32 -> Word16 -> Bool -> CUCTime
cucTimeFromBinary42 se m delta =
    let val1 :: Int32
        !val1 = fromIntegral se
        micro :: Double
        micro  = fromIntegral m
        micro' = round (micro / 65536.0 * 1000000.0)
    in  CUCTime Cuc42 (fromIntegral val1) micro' delta

{-# INLINABLE cucTimeFromBinary43 #-}
cucTimeFromBinary43 :: Word32 -> Word24 -> Bool -> CUCTime
cucTimeFromBinary43 se m delta =
    let val1 :: Int32
        !val1 = fromIntegral se
        micro :: Double
        micro  = fromIntegral m
        micro' = round (micro / 16777216.0 * 1000000.0)
    in  CUCTime Cuc43 (fromIntegral val1) micro' delta

{-# INLINABLE cucTimeFromBinary44 #-}
cucTimeFromBinary44 :: Word32 -> Word32 -> Bool -> CUCTime
cucTimeFromBinary44 se m delta =
    let val1 :: Int32
        !val1 = fromIntegral se
    in  CUCTime Cuc43 (fromIntegral val1) m delta


instance TimeRepConversion CUCTime where
    {-# INLINABLE timeToWord64 #-}
    timeToWord64 (CUCTime _ sec usec delta) =
        timeToWord64' sec (fromIntegral usec) delta
    {-# INLINABLE word64ToTime #-}
    word64ToTime val delta =
        let (sec, mic) = word64ToTime' val
        in  CUCTime Cuc42 sec (fromIntegral mic) delta

    {-# INLINABLE timeToMicro #-}
    timeToMicro (CUCTime _ sec usec delta) =
        timeToMicro' sec (fromIntegral usec) delta
    {-# INLINABLE microToTime #-}
    microToTime val delta =
        let (sec, mic) = microToTime' val
        in  CUCTime Cuc42 sec (fromIntegral mic) delta

microToCUC :: CucEncoding -> Int64 -> Bool -> CUCTime
microToCUC enc val delta =
    let (sec, mic) = microToTime' val
    in  CUCTime enc sec (fromIntegral mic) delta



instance DeltaTime CUCTime where
    {-# INLINABLE isDelta #-}
    isDelta (CUCTime _ _ _ delta) = delta
    {-# INLINABLE setDelta #-}
    setDelta val (CUCTime enc sec mic _) = CUCTime enc sec mic val

instance DeltaTime CDSTime where
    isDelta _ = False
    setDelta _ t = t


-- | convert a time into a epoch time
{-# INLINABLE cucTimeToEpochTime #-}
cucTimeToEpochTime :: Epoch -> CUCTime -> EpochTime
cucTimeToEpochTime ep t@(CUCTime _ _ _ delta) =
    let newmic = timeToMicro t in EpochTime newmic delta ep

{-# INLINABLE epochTimeToCUCTime #-}
epochTimeToCUCTime :: CucEncoding -> EpochTime -> CUCTime
epochTimeToCUCTime enc (EpochTime mic delta _) =
    let newsec = mic `quot` microSecInt
        newmic = fromIntegral $ mic `rem` microSecInt
    in  CUCTime enc newsec newmic delta

{-# INLINABLE sunTimeToCUCTime #-}
sunTimeToCUCTime :: Epoch -> CucEncoding -> SunTime -> CUCTime
sunTimeToCUCTime ep enc t = epochTimeToCUCTime enc $ sunTimeToEpochTime ep t

{-# INLINABLE cucTimeToSunTime #-}
cucTimeToSunTime :: Epoch -> CUCTime -> SunTime
cucTimeToSunTime ep = epochTimeToSunTime . cucTimeToEpochTime ep


{-# INLINABLE cdsTimeToEpochTime #-}
cdsTimeToEpochTime :: Epoch -> CDSTime -> EpochTime
cdsTimeToEpochTime ep (CDSTime days milli micro) = EpochTime mic False ep
  where
    sec =
        (fromIntegral days * secsInDay + fromIntegral (milli `quot` 1000))
            * microSecInt
    mic =
        sec
            + (fromIntegral $ (milli `rem` 1000) * 1000 + fromIntegral
                  (fromMaybe 0 micro)
              )

-- | convert a epoch time to a CDS time
{-# INLINABLE epochTimeToCDSTime #-}
epochTimeToCDSTime :: EpochTime -> CDSTime
epochTimeToCDSTime (EpochTime mic _ _) = CDSTime days milli micro
  where
    (!abss , !absm ) = (abs mic) `quotRem` microSecInt
    (days' , secs' ) = abss `quotRem` secsInDay
    (milli', micro') = absm `quotRem` 1000
    days             = fromIntegral days'
    milli            = fromIntegral $ secs' * 1000 + milli'
    micro            = Just (fromIntegral micro')


{-# INLINABLE sunTimeToCDSTime #-}
sunTimeToCDSTime :: Epoch -> SunTime -> CDSTime
sunTimeToCDSTime ep = epochTimeToCDSTime . sunTimeToEpochTime ep


{-# INLINABLE cdsTimeToSunTime #-}
cdsTimeToSunTime :: Epoch -> CDSTime -> SunTime
cdsTimeToSunTime ep = epochTimeToSunTime . cdsTimeToEpochTime ep
