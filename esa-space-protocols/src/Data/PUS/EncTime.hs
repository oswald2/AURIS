{-# LANGUAGE OverloadedStrings
    , NoImplicitPrelude
    , BangPatterns
    , DeriveGeneric
    , GeneralizedNewtypeDeriving
    , RecordWildCards
    , NumericUnderscores
    , FlexibleInstances
    , DeriveAnyClass
#-}
module Data.PUS.EncTime
    ( CUCTime(..)
    , CDSTime(..)
    , mkCUCTime
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
    , cucTimeLen
    , cucTimeFromBinary
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
    )
where

import           RIO                     hiding ( Builder )

import           ByteString.StrictBuilder

import           Data.Binary
import           Data.Aeson
import           Data.Bits
import           Data.Attoparsec.ByteString     ( Parser )
import qualified Data.Attoparsec.Binary        as A

import           Codec.Serialise

import           Protocol.SizeOf

import           General.Time
import           General.SetBitField
import           General.GetBitField
import           General.Types



-- | Time types. CUC Time is standard unix time with normal encoding of
-- | 4 bytes coards and 2 bytes fine time
data CUCTime = CUCTime !Int64 !Int32 !Bool
    deriving (Eq, Show, Read, Generic, NFData)

instance Binary CUCTime
instance Serialise CUCTime
instance FromJSON CUCTime
instance ToJSON CUCTime where
    toEncoding = genericToEncoding defaultOptions


-- | Time is encoded in CDS Time format.
data CDSTime = CDSTime !Word16 !Word32 (Maybe Word16)
    deriving (Eq, Show, Read, Generic, NFData)

instance Binary CDSTime
instance Serialise CDSTime
instance FromJSON CDSTime
instance ToJSON CDSTime where
    toEncoding = genericToEncoding defaultOptions

cucTimeLen :: Int
cucTimeLen = 6

instance SizeOf CUCTime where
    sizeof _ = cucTimeLen

instance SizeOf CDSTime where
    sizeof (CDSTime _ _ (Just _)) = 8
    sizeof (CDSTime _ _ Nothing ) = 6

instance Display CUCTime where
    display = displayShow

{-# INLINABLE mkCUCTime #-}
mkCUCTime :: Int64 -> Int32 -> Bool -> CUCTime
mkCUCTime sec usec delta =
    let (restsec, usec') = abs usec `quotRem` fromIntegral microSecInt
        sign             = if sec < 0 || usec < 0 then (-1) else 1
        newSec           = (abs sec + fromIntegral restsec)
        newMicro         = usec'
    in  CUCTime (sign * newSec) (fromIntegral sign * newMicro) delta


{-# INLINABLE cucTimeIsDelta #-}
cucTimeIsDelta :: CUCTime -> Bool
cucTimeIsDelta (CUCTime _ _ x) = x

{-# INLINABLE cucTimeSetDelta #-}
cucTimeSetDelta :: CUCTime -> Bool -> CUCTime
cucTimeSetDelta (CUCTime s m _) x = (CUCTime s m x)


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
withinEps eps (CUCTime secs1 micro1 delta1) (CUCTime secs2 micro2 delta2) =
    secs1
        == secs2
        && fromIntegral (abs (micro1 - micro2))
        <  eps
        && delta1
        == delta2


-- | the null time for standard unix time
{-# INLINABLE nullCUCTime #-}
nullCUCTime :: CUCTime
nullCUCTime = CUCTime 0 0 False
-- | the null time for a CDS time
{-# INLINABLE nullCDSTime #-}
nullCDSTime :: CDSTime
nullCDSTime = CDSTime 0 0 (Just 0)


-- | the null time for standard unix time
{-# INLINABLE nullCUCTimeRel #-}
nullCUCTimeRel :: CUCTime
nullCUCTimeRel = CUCTime 0 0 True


-- | one micro second in unix time
{-# INLINABLE oneMicroSecondCUC #-}
oneMicroSecondCUC :: CUCTime
oneMicroSecondCUC = CUCTime 0 1 True
-- | one micro second in CDS time
{-# INLINABLE oneMicroSecondCDS #-}
oneMicroSecondCDS :: CDSTime
oneMicroSecondCDS = CDSTime 0 0 (Just 1)





{-# INLINABLE cucTimeBuilder #-}
cucTimeBuilder :: CUCTime -> Builder
cucTimeBuilder (CUCTime sec mic _) =
    let (s, m) = toEncoded sec mic in word32BE s <> word16BE m


instance SetValue CUCTime where
    {-# INLINABLE setValue #-}
    setValue vec off endian (CUCTime sec mic _) = do
        let (s, m) = toEncoded sec mic
        setValue vec off       endian s
        setValue vec (off + 4) endian m

instance GetValue CUCTime where
    {-# INLINABLE getValue #-}
    getValue byts off BiE =
        let se = getValue byts off BiE
            m  = getValue byts (off + 4) BiE
        in  cucTimeFromBinary se m False
    getValue byts off LiE =
        let se = getValue byts (off + 2) LiE
            m  = getValue byts off LiE
        in  cucTimeFromBinary se m False


{-# INLINABLE cdsTimeBuilder #-}
cdsTimeBuilder :: CDSTime -> Builder
cdsTimeBuilder (CDSTime days milli micro) =
    word16BE days <> word32BE milli <> case micro of
        Just m  -> word16BE m
        Nothing -> mempty


-- {-# INLINABLE worker #-}
-- worker :: Integer -> Int32 -> Bool -> BitPut ()
-- worker sec mic _ = do
--     let (sec', mic') = worker' sec mic
--     B.putWord32be 32 sec'
--     B.putWord16be 16 mic'


{-# INLINABLE toEncoded #-}
toEncoded :: Int64 -> Int32 -> (Word32, Word16)
toEncoded sec mic =
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




{-# INLINABLE cucTimeParser #-}
cucTimeParser :: CUCTime -> Parser CUCTime
cucTimeParser (CUCTime _ _ delta) = do
    se <- A.anyWord32be
    m  <- A.anyWord16be
    pure (cucTimeFromBinary se m delta)

{-# INLINABLE cucTimeFromBinary #-}
cucTimeFromBinary :: Word32 -> Word16 -> Bool -> CUCTime
cucTimeFromBinary se m delta =
    let val :: Word64
        val = fromIntegral se `shiftL` 16 .|. fromIntegral m
        (s, val') =
                let s' = val .&. 0x800000000000 /= 0
                in  if s'
                        then (s', complement ((val .|. 0xFFFF000000000000) - 1))
                        else (s', val)
        sec = sign * fromIntegral (val' `shiftR` 16) .&. 0xFFFFFFFF
        micro :: Double
        micro  = fromIntegral (val' .&. 0xFFFF)
        micro' = sign * round (micro * 65536.0 * 1000000.0)
        sign   = if s then (-1) else 1
    in  CUCTime (sign * sec) (fromIntegral micro') delta


instance TimeRepConversion CUCTime where
    {-# INLINABLE timeToWord64 #-}
    timeToWord64 (CUCTime sec usec delta) = timeToWord64' sec usec delta
    {-# INLINABLE word64ToTime #-}
    word64ToTime val delta =
        let (sec, mic) = word64ToTime' val in CUCTime sec mic delta

    {-# INLINABLE timeToMicro #-}
    timeToMicro (CUCTime sec usec delta) = timeToMicro' sec usec delta
    {-# INLINABLE microToTime #-}
    microToTime val delta =
        let (sec, mic) = microToTime' val in CUCTime sec mic delta




instance DeltaTime CUCTime where
    {-# INLINABLE isDelta #-}
    isDelta (CUCTime _ _ delta) = delta
    {-# INLINABLE setDelta #-}
    setDelta val (CUCTime sec mic _) = CUCTime sec mic val

instance DeltaTime CDSTime where 
    isDelta _ = False 
    setDelta _ t = t


-- | convert a time into a epoch time
{-# INLINABLE cucTimeToEpochTime #-}
cucTimeToEpochTime :: Epoch -> CUCTime -> EpochTime
cucTimeToEpochTime ep t@(CUCTime _ _ delta) =
    let newmic = timeToMicro t in EpochTime newmic delta ep

{-# INLINABLE epochTimeToCUCTime #-}
epochTimeToCUCTime :: EpochTime -> CUCTime
epochTimeToCUCTime (EpochTime mic delta _) =
    let newsec = mic `quot` microSecInt
        newmic = fromIntegral $ mic `rem` microSecInt
    in  CUCTime newsec newmic delta

{-# INLINABLE sunTimeToCUCTime #-}
sunTimeToCUCTime :: Epoch -> SunTime -> CUCTime
sunTimeToCUCTime ep = epochTimeToCUCTime . sunTimeToEpochTime ep

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