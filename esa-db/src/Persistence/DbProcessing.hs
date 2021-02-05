module Persistence.DbProcessing
    ( DbBackend
    , startDbProcessing
    , storeTMFrame
    , storeTMFrames
    , allTMFrames
    ) where


import           RIO
-- import           UnliftIO.STM
import           Control.Concurrent.STM        as Conc
import           Control.Monad.Logger
import           Control.Monad.Trans.Resource

import           Database.Persist.Postgresql
import           Database.Persist.Sqlite

import           Data.DbConfig
import           Data.DbConfig.Postgres
import           Data.DbConfig.SQLite

import           Persistence.Definitions

dbQueueSize :: Natural
dbQueueSize = 10000

type DB a = ReaderT SqlBackend (NoLoggingT (ResourceT IO)) a


startDbProcessing :: DbConfig -> IO DbBackend
startDbProcessing cfg = do
    frameQueue <- Conc.newTBQueueIO dbQueueSize

    let db = DbBackend {
                _dbbTMFrameQueue = frameQueue
                }

    withDB cfg (runMigration migrateAll)

    let with = case cfgBackend cfg of 
                    PGConfig pgConfig -> withPostgres pgConfig
                    SQConfig sqConfig -> withSQLite sqConfig

    _          <- async (tmFrameStoreThread cfg frameQueue with)
    return db


newtype DbBackend = DbBackend {
    _dbbTMFrameQueue :: TBQueue DbTMFrame
    }


storeTMFrame :: DbBackend -> DbTMFrame -> IO () 
storeTMFrame DbBackend {..} frame = do 
    RIO.atomically $ writeTBQueue _dbbTMFrameQueue frame

storeTMFrames :: DbBackend -> [DbTMFrame] -> IO () 
storeTMFrames DbBackend {..} frames = do 
    RIO.atomically $ do 
        forM_ frames $ writeTBQueue _dbbTMFrameQueue

allTMFrames :: DbConfig -> DbBackend -> IO [DbTMFrame]
allTMFrames cfg _backend = do 
    withDB cfg $ do 
        map entityVal <$> selectList [] []


withDB :: DbConfig -> DB a -> IO a
withDB DbConfig { cfgBackend = PGConfig pgConfig } action = do
    
    withPostgres pgConfig action
withDB DbConfig { cfgBackend = SQConfig sqConfig } action = do
    withSQLite sqConfig action


withPostgres
    :: PostgresConfig
    -> DB a
    -> IO a
withPostgres pgConfig =
    runStderrLoggingT
        . withPostgresqlPool (getConnString pgConfig)
                             (fromIntegral (pgNumberConns pgConfig))
        . liftSqlPersistMPool


withSQLite
    :: SQLiteConfig -> DB a -> IO a
withSQLite sqConfig =
    runStderrLoggingT
        . withSqlitePool (sqConnString sqConfig)
                         (fromIntegral (sqNumberConns sqConfig))
        . liftSqlPersistMPool



tmFrameStoreThread :: DbConfig -> TBQueue DbTMFrame -> (DB () -> IO a) -> IO () 
tmFrameStoreThread _cfg frameQueue with = do
    forever $ do
        frames <- Conc.atomically (flushTBQueue frameQueue)
        with $ insertMany_ frames




-- data TMFrameQuery = 
--     AllTMFrames 
--     | TMFrameRange SunTime SunTime


-- tmFrameQueryThread :: TBQueue TMFrameQuery -> (DB a -> IO a) -> IO ()
-- tmFrameQueryThread queue with = do 
--     forever $ do 
--         query <- Conc.atomically (readTBQueue queue)
--         process query 
--     where 
--         process AllTMFrames = 
