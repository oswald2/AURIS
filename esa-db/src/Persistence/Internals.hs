{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}

module Persistence.Internals
  ( withDatabaseWriter
  , databaseSink
  , Table' (..)
  )
where

import           RIO
import           Conduit

import           Database.Selda
import           Database.Selda.Backend.Internal ( runSeldaT )
import           Database.Selda.SQLite           ( sqliteOpen, seldaClose )

import           Control.Concurrent.STM.TBQueue  ( flushTBQueue )


-- | Opens DB connection and provides you with a function to insert records.
-- Internally it creates a queue and spawns a thread so insertions are
-- performed asynchronously.
withDatabaseWriter
  :: forall m n a b . (MonadUnliftIO m, MonadMask m, MonadIO n, Table' a)
  => FilePath -> ((a -> n ()) -> m b) -> m b
withDatabaseWriter dbPath app =
  bracket (sqliteOpen dbPath) seldaClose $ \connection -> do
    let runSelda f = runSeldaT f connection
    runSelda $ tryCreateTable (table' :: Table a)

    q <- newTBQueueIO 256

    let writeFn = atomically . writeTBQueue q

    -- Without masking we can loose records if exeption occurs after reading
    -- them from the queue but before inserting them into the table.
    -- Also we should be careful to unmask exceptions when reading from the
    -- queue to allow parent thread to safely interrupt waiting and break the
    -- loop.
    let writeThread = mask $ \unmask -> forever $ do
          x <- unmask $ atomically $ readTBQueue q
          xs <- atomically $ flushTBQueue q
          runSelda $ insert_ table' $ x:xs
    let writeFlush = mask_ $ runSelda $ do
          xs <- atomically $ flushTBQueue q
          insert_ table' xs

    -- We 'link' exeptions from the child thread so they are propagated to the
    -- main thread. This prevents from silent DB crashes.
    -- Remaining records are inserted synchronously so we don't loose anything
    -- in case of unexpected program exit.
    withAsync writeThread (\th -> link th >> app writeFn) `finally` writeFlush



-- | Provides Conduit sink that inserts incoming records into DB table.
databaseSink
  :: forall m a o . (MonadResource m, Table' a)
  => FilePath -> ConduitT a o m ()
databaseSink dbPath =
  bracketP seldaOpen (seldaClose . fst) $ \(_, selda) ->
    awaitForever $ \x -> liftIO $ selda $ insert_ table' [x]
  where
    seldaOpen = do
      conn <- sqliteOpen dbPath
      let selda f = runSeldaT f conn
      selda $ tryCreateTable (table' :: Table a)
      return (conn, selda)



-- | This wrapper class exists only hide dependency on selda from users of
-- esa-db package. Without it we need to export table definition (like 'logEntries :: Table a').
--
-- NB. Actually this may be not the best design decision as it is possible to
-- have several valid instnces for a single type. E.g. we may want to store
-- TMFrames in two different tables.
class Relational a => Table' a where
  table' :: Table a
