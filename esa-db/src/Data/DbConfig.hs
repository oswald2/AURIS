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
    cfgDbDebugLog    :: !Bool
    , cfgBackend       :: !DbBackendConfig
    }
    deriving (Eq, Read, Show, Generic, FromJSON, ToJSON)


data DbBackendConfig =
  NoDB
  | PGConfig PostgresConfig
  | SQConfig SQLiteConfig
  deriving(Eq, Read, Show, Generic, FromJSON, ToJSON)



defaultDbConfig :: DbConfig
defaultDbConfig = DbConfig { cfgDbDebugLog = True
                           , cfgBackend       = PGConfig defaultPostgresConfig
                           }


