module Persistence.DbProcessing
    ( DbBackend
    , startDbProcessing
    , storeTMFrame
    , storeTMFrames
    , eventsLastPage
    , allTMFrames
    , tmFramesLastPage
    , storeLog

    , withDB
    , withPostgres
    , withPostgresDebug 
    , withSQLite
    , withSQLiteDebug
    ) where


import           RIO
import qualified Data.Text.IO                  as T
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

data DbBackend = DbBackend
    { _dbbTMFrameQueue :: TBQueue DbTMFrame
    , _dbbEventQueue   :: TBQueue DbLogEvent
    }



startDbProcessing :: DbConfig -> IO (Maybe DbBackend)
startDbProcessing DbConfig { cfgBackend = NoDB } = return Nothing
startDbProcessing cfg = do
    frameQueue <- Conc.newTBQueueIO dbQueueSize
    eventQueue <- Conc.newTBQueueIO dbQueueSize

    let
        db = DbBackend { _dbbTMFrameQueue = frameQueue
                       , _dbbEventQueue   = eventQueue
                       }

    T.putStrLn "Running DB migrations..."
    withDB cfg (runMigration migrateAll)
    T.putStrLn "Migrations done."

    let with = case cfgBackend cfg of
            PGConfig pgConfig -> withPostgres pgConfig
            SQConfig sqConfig -> withSQLite sqConfig
            _ -> \_ -> return ()

    let tmFramesT = conc $ tmFrameStoreThread cfg frameQueue with
        eventsT   = conc $ eventStoreThread cfg eventQueue with

        threads   = tmFramesT <> eventsT

    -- start all the threads    
    _ <- async $ runConc threads

    -- return the backend 
    return (Just db)




storeLog :: DbBackend -> DbLogEvent -> IO ()
storeLog DbBackend {..} event = do
    RIO.atomically $ writeTBQueue _dbbEventQueue event

eventsLastPage :: DbConfig -> Int -> Int -> IO [DbLogEvent]
eventsLastPage cfg pageSize offset = do
    withDB cfg $ do
        map entityVal <$> selectList
            []
            [Desc DbLogEventTimestamp, LimitTo pageSize, OffsetBy offset]

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

tmFramesLastPage :: DbConfig -> Int -> Int -> IO [DbTMFrame]
tmFramesLastPage cfg pageSize offset = do
    withDB cfg $ do
        map entityVal <$> selectList
            []
            [Desc DbTMFrameErt, LimitTo pageSize, OffsetBy offset]


withDB :: DbConfig -> DB a -> IO a
withDB DbConfig { cfgDbDebugLog = doLog, cfgBackend = PGConfig pgConfig } action
    = do
        if doLog
            then withPostgresDebug pgConfig action
            else withPostgres pgConfig action

withDB DbConfig { cfgDbDebugLog = doLog, cfgBackend = SQConfig sqConfig } action
    = do
        if doLog
            then withSQLiteDebug sqConfig action
            else withSQLite sqConfig action
withDB _ _ = fail "Error: withDB called with not-supported DB backend"

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


withSQLite :: SQLiteConfig -> DB a -> IO a
withSQLite sqConfig =
    runNoLoggingT
        . withSqlitePool (sqConnString sqConfig)
                         (fromIntegral (sqNumberConns sqConfig))
        . liftSqlPersistMPool

withSQLiteDebug :: SQLiteConfig -> DB a -> IO a
withSQLiteDebug sqConfig =
    runStderrLoggingT
        . withSqlitePool (sqConnString sqConfig)
                         (fromIntegral (sqNumberConns sqConfig))
        . liftSqlPersistMPool


tmFrameStoreThread :: DbConfig -> TBQueue DbTMFrame -> (DB () -> IO a) -> IO ()
tmFrameStoreThread _cfg frameQueue with = do
    forever $ do
        frames <- Conc.atomically (flushTBQueue frameQueue)
        with $ insertMany_ frames


eventStoreThread :: DbConfig -> TBQueue DbLogEvent -> (DB () -> IO a) -> IO ()
eventStoreThread _cfg eventQueue with = do
    forever $ do
        events <- Conc.atomically (flushTBQueue eventQueue)
        with $ insertMany_ events


