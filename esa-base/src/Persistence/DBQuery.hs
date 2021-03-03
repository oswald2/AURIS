module Persistence.DBQuery
    ( DBQuery(..)
    , DBResult(..)
    ) where

import           RIO

import           Codec.Serialise
import           Data.Aeson
import           General.Time

import           Data.PUS.TMFrame               ( TMFrame )
import           Data.PUS.ExtractedDU


data DBQuery = DbGetTMFrames
    { dbFromTime :: Maybe SunTime
    , dbToTime   :: Maybe SunTime
    }
    deriving (Show, Generic)


instance Serialise DBQuery
instance FromJSON DBQuery
instance ToJSON DBQuery where
    toEncoding = genericToEncoding defaultOptions


data DBResult =
    DBResultTMFrames [ExtractedDU TMFrame]
    | DBResultEvents
    deriving (Show, Generic)


instance Serialise DBResult
instance FromJSON DBResult
instance ToJSON DBResult where
    toEncoding = genericToEncoding defaultOptions
