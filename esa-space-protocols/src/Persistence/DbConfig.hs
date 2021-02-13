module Data.DbConfig
    ( DbConfig(..)
    , DbBackendConfig(..)
    , defaultDbConfig
    ) where

import           RIO

import           Data.Aeson

import           Data.DbConfig.Postgres
import           Data.DbConfig.SQLite



data DbConfig = DbConfig
    {
    -- | Flag to specify if TM Frames should be stored in the database
      cfgStoreTMFrames :: !Bool
    , cfgDbDebugLog    :: !Bool
    , cfgBackend       :: !DbBackendConfig
    }
    deriving (Eq, Read, Show, Generic, FromJSON, ToJSON)


data DbBackendConfig =
  NoDB
  | PGConfig PostgresConfig
  | SQConfig SQLiteConfig
  | MongoDB DbConfigMongoDB
  deriving(Eq, Read, Show, Generic, FromJSON, ToJSON)



defaultDbConfig :: DbConfig
defaultDbConfig = DbConfig { cfgStoreTMFrames = True
                           , cfgDbDebugLog = True
                           , cfgBackend       = PGConfig defaultPostgresConfig
                           }


