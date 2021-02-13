module Data.DbConfig.MongoDB
    ( DbConfigMongoDB(..)
    , defaultMongoDBConfig
    ) where


import           RIO

import           Data.Aeson


data DbConfigMongoDB = DbConfigMongoDB
    { cfgMongoHost :: !Text
    , cfgMongoPort :: Maybe Text
    }
    deriving (Eq, Show, Read, Generic)

instance FromJSON DbConfigMongoDB
instance ToJSON DbConfigMongoDB where
    toEncoding = genericToEncoding defaultOptions


defaultMongoDBConfig :: DbConfigMongoDB
defaultMongoDBConfig =
    DbConfigMongoDB { cfgMongoHost = "localhost", cfgMongoPort = Nothing }


