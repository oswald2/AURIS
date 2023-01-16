module Conduit.Extras
    ( queueSource
    , queueSink
    , queueFlusher
    , queuePusher
    , readWithTimeout
    ) where

import           Conduit
import           RIO

queueSource :: (MonadIO m) => TBQueue a -> ConduitT () a m ()
queueSource queue = do
    val <- atomically $ readTBQueue queue
    yield val
    queueSource queue


queueSink :: (MonadIO m) => TBQueue a -> ConduitT a Void m ()
queueSink queue = awaitForever $ \val -> atomically (writeTBQueue queue val)


queueFlusher :: (MonadIO m) => TBQueue a -> ConduitT z [a] m ()
queueFlusher queue = forever $ liftIO (atomically action) >>= yield
  where
    action = do
        v  <- readTBQueue queue
        vs <- flushTBQueue queue
        return (v : vs)

queuePusher :: (MonadIO m) => TBQueue a -> ConduitT [a] Void m ()
queuePusher queue = awaitForever $ \lst -> do
    atomically $ forM_ lst $ writeTBQueue queue


readWithTimeout :: (MonadIO m) => Int -> TBQueue a -> m (Maybe a)
readWithTimeout tout queue = do
    delay <- liftIO $ registerDelay tout
    atomically
        $   do
                Just <$> readTBQueue queue
        <|> Nothing
        <$  delayCheck delay
    where delayCheck = checkSTM <=< readTVar
