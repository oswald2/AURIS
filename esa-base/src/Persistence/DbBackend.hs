module Persistence.DbBackend
    ( DbBackend
    , DbBackendClass(..)
    , createDbBackend
    , storeLog
    , storeTMFrame
    , storeTMFrames
    , storePUSPacket
    , DbLogFunc
    , stopDbProcessing
    ) where

import           RIO
import qualified Data.Text.IO                  as T
import           Data.PUS.TMStoreFrame
import           Data.PUS.ExtractedDU
import           Data.PUS.PUSPacket

import           Persistence.LogEvent           ( LogEvent )


type DbLogFunc = LogSource -> LogLevel -> Utf8Builder -> IO ()


-- | This class is to be used for functions directly querying the DB and 
-- returning a result. 
class DbBackendClass a m where
    allTMFrames :: (MonadUnliftIO m, MonadReader env m, HasLogFunc env) => a -> m [TMStoreFrame]
    dropTMFramesTable :: (MonadUnliftIO m, MonadReader env m, HasLogFunc env) => a -> m ()

-- | This is the interface for asynchronous writes to the database (streaming)
data DbBackend = DbBackend
    { _dbbTMFrameQueue   :: TBQueue TMStoreFrame
    , _dbbEventQueue     :: TBQueue LogEvent
    , _dbbPUSPacketQueue :: TBQueue (ExtractedDU PUSPacket)
    , _dbbThread         :: Async ()
    }


createDbBackend
    :: (Monad m)
    => TBQueue TMStoreFrame
    -> TBQueue LogEvent
    -> TBQueue (ExtractedDU PUSPacket)
    -> Async ()
    -> m DbBackend
createDbBackend frameQueue eventQueue packetQueue thread = return DbBackend
    { _dbbTMFrameQueue   = frameQueue
    , _dbbEventQueue     = eventQueue
    , _dbbPUSPacketQueue = packetQueue
    , _dbbThread         = thread
    }


storeLog :: (MonadIO m) => DbBackend -> LogEvent -> m ()
storeLog backend le = do
    atomically $ writeTBQueue (_dbbEventQueue backend) le

storeTMFrame :: (MonadIO m) => DbBackend -> TMStoreFrame -> m ()
storeTMFrame backend frame = do
    liftIO $ T.putStrLn "storeTMFrame: sending frame to DB..."
    atomically $ writeTBQueue (_dbbTMFrameQueue backend) frame

storeTMFrames :: (MonadIO m) => DbBackend -> [TMStoreFrame] -> m ()
storeTMFrames backend frames = do
    atomically $ do
        mapM_ (writeTBQueue (_dbbTMFrameQueue backend)) frames

storePUSPacket :: (MonadIO m) => DbBackend -> ExtractedDU PUSPacket -> m ()
storePUSPacket backend pkt = do
    liftIO $ T.putStrLn "storePUSPacket: sending packet to DB..."
    atomically $ writeTBQueue (_dbbPUSPacketQueue backend) pkt

stopDbProcessing
    :: (MonadIO m, MonadReader env m, HasLogFunc env) => DbBackend -> m ()
stopDbProcessing backend = do
    logWarn "Stopping database backend..."
    cancel (_dbbThread backend)
    logWarn "Stopping database backend..."

