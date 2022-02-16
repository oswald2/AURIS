module Persistence.DBQuery
    ( DBQuery(..)
    , DbGetFrameRange(..)
    , DbGetLastNFrames(..)
    , DbGetNextNFrames(..)
    , DBResult(..)
    ) where

import           RIO

import           Codec.Serialise
import           Data.Aeson
import           General.Time

import           Data.PUS.TMFrame               ( TMFrame )
import           Data.PUS.ExtractedDU


data DBQuery =
    FrRange DbGetFrameRange
    | FrPrev DbGetLastNFrames
    | FrNext DbGetNextNFrames
    deriving (Show, Generic)

instance Serialise DBQuery
instance FromJSON DBQuery
instance ToJSON DBQuery where
    toEncoding = genericToEncoding defaultOptions


data DbGetFrameRange = DbGetFrameRange {
    dbFromTime :: Maybe SunTime
    , dbToTime   :: Maybe SunTime
    }
    deriving (Show, Generic)

instance Serialise DbGetFrameRange
instance FromJSON DbGetFrameRange
instance ToJSON DbGetFrameRange where
    toEncoding = genericToEncoding defaultOptions


data DbGetLastNFrames = DbGetLastNFrames { 
    dbStart :: !SunTime
    , dbN :: !Word32 
    }
    deriving (Show, Generic)

instance Serialise DbGetLastNFrames
instance FromJSON DbGetLastNFrames
instance ToJSON DbGetLastNFrames where
    toEncoding = genericToEncoding defaultOptions

data DbGetNextNFrames = DbGetNextNFrames { 
    dbnStart :: !SunTime
    , dbnN :: !Word32 
    }
    deriving (Show, Generic)

instance Serialise DbGetNextNFrames
instance FromJSON DbGetNextNFrames
instance ToJSON DbGetNextNFrames where
    toEncoding = genericToEncoding defaultOptions


data DBResult =
    DBResultTMFrames [ExtractedDU TMFrame]
    | DBResultTMFramesFinished
    | DBResultEvents
    deriving (Show, Generic)


instance Serialise DBResult
instance FromJSON DBResult
instance ToJSON DBResult where
    toEncoding = genericToEncoding defaultOptions
