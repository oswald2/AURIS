module Persistence.DbProcessing
    ( DbBackend
    , startDbProcessing
    -- , storeTMFrame
    -- , storeTMFrames
    , eventsLastPage
    , tmFramesLastPage
    -- , storeLog
    -- , withDB
    , withPostgres
    , withPostgresDebug
    -- , withSQLite
    -- , withSQLiteDebug
    ) where


import           RIO
import qualified Data.Text.IO                  as T
import           Control.Concurrent.STM        as Conc
import           Control.Monad.Logger
import           Control.Monad.Trans.Resource

import           Database.Persist.Postgresql
-- import           Database.Persist.Sqlite

import           Data.DbConfig.Postgres
-- import           Data.DbConfig.SQLite

import           Persistence.Definitions
import           Persistence.DbBackend

import           Data.PUS.TMStoreFrame

import           Persistence.LogEvent
import           Persistence.Conversion.Types
import           Persistence.Conversion.TMFrame ( )
import           Persistence.Conversion.LogEvent
                                                ( )



dbQueueSize :: Natural
dbQueueSize = 10000

type DB a = ReaderT SqlBackend (NoLoggingT (ResourceT IO)) a



startDbProcessing :: PostgresConfig -> IO DbBackend
startDbProcessing cfg = do
    frameQueue <- Conc.newTBQueueIO dbQueueSize
    eventQueue <- Conc.newTBQueueIO dbQueueSize

    T.putStrLn "Running DB migrations..."
    withPostgresDebug cfg (runMigration migrateAll)
    T.putStrLn "Migrations done."

    let tmFramesT = conc $ tmFrameStoreThread cfg frameQueue
        eventsT   = conc $ eventStoreThread cfg eventQueue

        threads   = tmFramesT <> eventsT

    -- start all the threads    
    t <- async $ runConc threads

    createDbBackend 
        frameQueue 
        eventQueue 
        t
        (getAllTMFrames cfg)


eventsLastPage :: PostgresConfig -> Int -> Int -> IO [DbLogEvent]
eventsLastPage cfg pageSize offset = do
    withPostgresDebug cfg $ do
        map entityVal <$> selectList
            []
            [Desc DbLogEventTimestamp, LimitTo pageSize, OffsetBy offset]

getAllTMFrames :: PostgresConfig -> IO [TMStoreFrame]
getAllTMFrames cfg = do
    withPostgresDebug cfg $ do
        map (fromDB . entityVal) <$> selectList [] []

tmFramesLastPage :: PostgresConfig -> Int -> Int -> IO [DbTMFrame]
tmFramesLastPage cfg pageSize offset = do
    withPostgresDebug cfg $ do
        map entityVal <$> selectList
            []
            [Desc DbTMFrameErt, LimitTo pageSize, OffsetBy offset]


-- withDB :: DbConfig -> DB a -> IO a
-- withDB DbConfig { cfgDbDebugLog = doLog, cfgBackend = PGConfig pgConfig } action
--     = do
--         if doLog
--             then withPostgresDebug pgConfig action
--             else withPostgres pgConfig action

-- withDB DbConfig { cfgDbDebugLog = doLog, cfgBackend = SQConfig sqConfig } action
--     = do
--         if doLog
--             then withSQLiteDebug sqConfig action
--             else withSQLite sqConfig action
-- withDB _ _ = fail "Error: withDB called with not-supported DB backend"

withPostgres :: PostgresConfig -> DB a -> IO a
withPostgres pgConfig =
    runNoLoggingT
        . withPostgresqlPool (getConnString pgConfig)
                             (fromIntegral (pgNumberConns pgConfig))
        . liftSqlPersistMPool

withPostgresDebug :: PostgresConfig -> DB a -> IO a
withPostgresDebug pgConfig =
    runStderrLoggingT
        . withPostgresqlPool (getConnString pgConfig)
                             (fromIntegral (pgNumberConns pgConfig))
        . liftSqlPersistMPool


-- withSQLite :: SQLiteConfig -> DB a -> IO a
-- withSQLite sqConfig =
--     runNoLoggingT
--         . withSqlitePool (sqConnString sqConfig)
--                          (fromIntegral (sqNumberConns sqConfig))
--         . liftSqlPersistMPool

-- withSQLiteDebug :: SQLiteConfig -> DB a -> IO a
-- withSQLiteDebug sqConfig =
--     runStderrLoggingT
--         . withSqlitePool (sqConnString sqConfig)
--                          (fromIntegral (sqNumberConns sqConfig))
--         . liftSqlPersistMPool


tmFrameStoreThread :: PostgresConfig -> TBQueue TMStoreFrame -> IO ()
tmFrameStoreThread cfg frameQueue = do
    runNoLoggingT . withPostgresqlConn (getConnString cfg) $ \backend -> do
        forever $ do
            frames <- liftIO $ Conc.atomically (flushTBQueue frameQueue)
            runSqlConn (insertMany_ (map toDB frames)) backend


eventStoreThread :: PostgresConfig -> TBQueue LogEvent -> IO ()
eventStoreThread cfg eventQueue = do
    runNoLoggingT $ withPostgresqlConn (getConnString cfg) $ \backend -> do
        forever $ do
            events <- liftIO $ Conc.atomically (flushTBQueue eventQueue)
            runSqlConn (insertMany_ (map toDB events)) backend


