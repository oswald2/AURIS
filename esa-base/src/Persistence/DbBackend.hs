module Persistence.DbBackend
    ( DbBackend
    , createDbBackend
    , storeLog
    , storeTMFrame
    , storeTMFrames
    , allTMFrames
    ) where

import           RIO
import           Data.PUS.TMStoreFrame

import           Persistence.LogEvent           ( LogEvent )


data DbBackend = DbBackend
    { _dbbTMFrameQueue :: TBQueue TMStoreFrame
    , _dbbEventQueue   :: TBQueue LogEvent
    , _dbbThread       :: Async ()
    , dbbAllFrames     :: IO [TMStoreFrame]
    }


createDbBackend
    :: (Monad m)
    => TBQueue TMStoreFrame
    -> TBQueue LogEvent
    -> Async ()
    -> IO [TMStoreFrame]
    -> m DbBackend
createDbBackend frameQueue eventQueue thread getAllFrames = return DbBackend
    { _dbbTMFrameQueue = frameQueue
    , _dbbEventQueue   = eventQueue
    , _dbbThread       = thread
    , dbbAllFrames     = getAllFrames
    }


storeLog :: (MonadIO m) => DbBackend -> LogEvent -> m ()
storeLog backend le = do
    atomically $ writeTBQueue (_dbbEventQueue backend) le

storeTMFrame :: (MonadIO m) => DbBackend -> TMStoreFrame -> m ()
storeTMFrame backend frame = do
    atomically $ writeTBQueue (_dbbTMFrameQueue backend) frame

storeTMFrames :: (MonadIO m) => DbBackend -> [TMStoreFrame] -> m ()
storeTMFrames backend frames = do
    atomically $ do
      mapM_ (writeTBQueue (_dbbTMFrameQueue backend)) frames


allTMFrames :: (MonadIO m) => DbBackend -> m [TMStoreFrame]
allTMFrames = liftIO . dbbAllFrames