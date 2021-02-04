module Data.DbConfig
    ( DbConfig(..)
    , defaultDbConfig
    ) where

import           RIO

import           Data.Aeson

import           Data.DbConfig.Postgres
import           Data.DbConfig.SQLite

data DbConfig =
  PGConfig PostgresConfig
  | SQConfig SQLiteConfig
  deriving(Eq, Read, Show, Generic, FromJSON, ToJSON)



defaultDbConfig :: DbConfig 
defaultDbConfig = PGConfig defaultPostgresConfig

