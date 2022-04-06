{-# LANGUAGE  TemplateHaskell, DeriveAnyClass #-}
module Data.PUS.Statistics
    ( Statistics(..)
    , statNewDU
    , statN
    , statBytes
    , statFirst
    , initialStatistics
    , statCalc
    , statTotal
    , statCurrent
    , statCurrentTime
    , TMFrameStats(..)
    , TMPacketStats(..)
    , TMStatistics(..)
    , TimeStamp(..)
    , nullTimeStamp
    , toUTCTime
    ) where

import           RIO                     hiding ( (^.)
                                                , to
                                                )

import           Control.Lens

import           Codec.Serialise
import           Data.Aeson

import           Data.Time.Clock.POSIX
import qualified Data.Time.Clock               as DT



data Statistics = Statistics
    { _statN       :: !Int64
    , _statBytes   :: !Int64
    , _statFirst   :: Maybe POSIXTime
    , _statCurrent :: Maybe POSIXTime
    }
makeLenses ''Statistics

initialStatistics :: Statistics
initialStatistics = Statistics { _statN       = 0
                               , _statBytes   = 0
                               , _statFirst   = Nothing
                               , _statCurrent = Nothing
                               }


statCurrentTime :: Statistics -> TimeStamp
statCurrentTime st =
    TimeStamp $ fromMaybe (0) (_statCurrent st)

statNewDU :: POSIXTime -> Int64 -> Statistics -> Statistics
statNewDU now size s =
    s & statN +~ 1 & statBytes +~ size & case s ^. statFirst of
        Nothing -> statFirst ?~ now
        Just _  -> statCurrent ?~ now


statCalc :: Statistics -> Statistics -> (Double, Double)
statCalc !s1 !s2 =
    let bytes = s2 ^. statBytes - s1 ^. statBytes
        n     = s2 ^. statN - s1 ^. statN
    in  case (s1 ^. statCurrent, s2 ^. statCurrent) of
            (Just t1, Just t2) ->
                let diff      = t2 - t1
                    !duRate   = fromIntegral n / realToFrac diff
                    !dataRate = fromIntegral bytes / realToFrac diff
                in  if diff == 0.0 then (0, 0) else (duRate, dataRate)
            _ -> (0.0, 0.0)

statTotal :: Statistics -> (Double, Double)
statTotal s = case (s ^. statFirst, s ^. statCurrent) of
    (Just start, Just end) ->
        let diff         = end - start
            !duTotal     = fromIntegral (s ^. statN) / realToFrac diff
            !duTotalRate = fromIntegral (s ^. statBytes) / realToFrac diff
        in  if diff == 0 then (0, 0) else (duTotal, duTotalRate)
    _ -> (0, 0)


newtype TimeStamp = TimeStamp POSIXTime
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)



toUTCTime :: TimeStamp -> DT.UTCTime
toUTCTime (TimeStamp t) = posixSecondsToUTCTime t

nullTimeStamp :: TimeStamp
nullTimeStamp = TimeStamp 0

instance Serialise TimeStamp where
    encode (TimeStamp t) = Codec.Serialise.encode (posixSecondsToUTCTime t)
    decode = TimeStamp . utcTimeToPOSIXSeconds <$> Codec.Serialise.decode

-- instance FromJSON TimeStamp where
--     parseJSON (Number n) =
--         let v :: Int64
--             v = fromMaybe 0 (toBoundedInteger n)
--         in  pure $ TimeStamp (v ^. from microseconds)
--     parseJSON invalid = E.prependFailure "parsing TimeStamp failed, "
--                                          (E.typeMismatch "Number" invalid)

-- instance ToJSON TimeStamp where
--     toJSON (TimeStamp t) = Number (fromIntegral (t ^. microseconds))
--     toEncoding (TimeStamp t) = E.int64 (t ^. microseconds)

data TMFrameStats = TMFrameStats
    { tmStatFrameTotal      :: !Double
    , tmStatFrameTotalBytes :: !Double
    , tmStatFrameRate       :: !Double
    , tmStatFrameBytes      :: !Double
    , tmStatFramesN         :: !Int64
    , tmStatFrameTime       :: !TimeStamp
    }
    deriving (Show, Generic)

instance Serialise TMFrameStats
instance FromJSON TMFrameStats
instance ToJSON TMFrameStats where
    toEncoding = genericToEncoding defaultOptions

data TMPacketStats = TMPacketStats
    { tmStatPacketTotal      :: !Double
    , tmStatPacketTotalBytes :: !Double
    , tmStatPacketRate       :: !Double
    , tmStatPacketBytes      :: !Double
    , tmStatPacketsN         :: !Int64
    , tmStatPacketTime       :: !TimeStamp
    }
    deriving (Show, Generic)


instance Serialise TMPacketStats
instance FromJSON TMPacketStats
instance ToJSON TMPacketStats where
    toEncoding = genericToEncoding defaultOptions


data TMStatistics = TMStatistics
    { _statFrame   :: !TMFrameStats
    , _statPackets :: !TMPacketStats
    }
    deriving (Show, Generic)

instance Serialise TMStatistics
instance FromJSON TMStatistics
instance ToJSON TMStatistics where
    toEncoding = genericToEncoding defaultOptions
