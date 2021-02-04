module Persistence.DbInterface
    ( withDB
    ) where


import           RIO

import           Control.Monad.Logger

import           Data.Pool

import           Database.Persist.Postgresql
import           Database.Persist.Sqlite

import           Data.DbConfig
import           Data.DbConfig.Postgres
import           Data.DbConfig.SQLite


withDB
    :: (MonadUnliftIO m, MonadLogger m)
    => DbConfig
    -> (Pool SqlBackend -> m a)
    -> m a
withDB (PGConfig pgConfig) action = do
    withPostgresqlPool (getConnString pgConfig)
                       (fromIntegral (pgNumberConns pgConfig))
                       action
withDB (SQConfig sqConfig) action = do
    withSqlitePool (sqConnString sqConfig)
                   (fromIntegral (sqNumberConns sqConfig))
                   action
