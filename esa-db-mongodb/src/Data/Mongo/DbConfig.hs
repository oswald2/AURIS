module Data.Mongo.DbConfig
    ( DbConfigMongoDB(..)
    , defaultMongoDBConfig
    ) where


import           RIO



data DbConfigMongoDB = DbConfigMongoDB {
    cfgMongoHost :: !Text 
    , cfgMongoPort :: Maybe Text 
  }


defaultMongoDBConfig :: DbConfigMongoDB 
defaultMongoDBConfig = DbConfigMongoDB {
    cfgMongoHost = "localhost"
    , cfgMongoPort = Nothing
  }
