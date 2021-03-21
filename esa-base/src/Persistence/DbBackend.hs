module Persistence.DbBackend
    ( DbBackend
    , DbBackendClass(..)
    , createDbBackend
    , storeLog
    , storeTMFrame
    , storeTMFrames
    , storePUSPacket
    , storeTMPacket
    , DbLogFunc
    , stopDbProcessing
    , rememberQueryThread
    ) where

import           RIO
-- import qualified Data.Text.IO                  as T
import           Data.PUS.TMFrame
import           Data.PUS.ExtractedDU
import           Data.PUS.PUSPacket
import           Data.PUS.TMPacket

import           Persistence.LogEvent           ( LogEvent )


type DbLogFunc = LogSource -> LogLevel -> Utf8Builder -> IO ()


-- | This class is to be used for functions directly querying the DB and 
-- returning a result. 
class DbBackendClass a m where
    allTMFrames :: (MonadUnliftIO m, MonadReader env m, HasLogFunc env) => a -> m [(ExtractedDU TMFrame)]
    dropTMFramesTable :: (MonadUnliftIO m, MonadReader env m, HasLogFunc env) => a -> m ()

-- | This is the interface for asynchronous writes to the database (streaming)
data DbBackend = DbBackend
    { _dbbTMFrameQueue   :: TBQueue (ExtractedDU TMFrame)
    , _dbbEventQueue     :: TBQueue LogEvent
    , _dbbPUSPacketQueue :: TBQueue (ExtractedDU PUSPacket)
    , _dbbTMPacketQueue  :: TBQueue TMPacket
    , _dbbThread         :: Async ()
    , _dbbQueryThread    :: TVar (Maybe (Async ()))
    }


createDbBackend
    :: (MonadUnliftIO m)
    => TBQueue (ExtractedDU TMFrame)
    -> TBQueue LogEvent
    -> TBQueue (ExtractedDU PUSPacket)
    -> TBQueue TMPacket
    -> Async ()
    -> m DbBackend
createDbBackend frameQueue eventQueue packetQueue tmPacketQueue thread = do
    var <- newTVarIO Nothing
    return DbBackend { _dbbTMFrameQueue   = frameQueue
                     , _dbbEventQueue     = eventQueue
                     , _dbbPUSPacketQueue = packetQueue
                     , _dbbTMPacketQueue  = tmPacketQueue
                     , _dbbThread         = thread
                     , _dbbQueryThread    = var
                     }


rememberQueryThread :: (MonadIO m) => DbBackend -> Async () -> m () 
rememberQueryThread backend t = do 
    atomically $ writeTVar (_dbbQueryThread backend) (Just t)

storeLog :: (MonadIO m) => DbBackend -> LogEvent -> m ()
storeLog backend le = do
    atomically $ writeTBQueue (_dbbEventQueue backend) le

storeTMFrame :: (MonadIO m) => DbBackend -> (ExtractedDU TMFrame) -> m ()
storeTMFrame backend frame = do
    atomically $ writeTBQueue (_dbbTMFrameQueue backend) frame

storeTMFrames :: (MonadIO m) => DbBackend -> [(ExtractedDU TMFrame)] -> m ()
storeTMFrames backend frames = do
    atomically $ do
        mapM_ (writeTBQueue (_dbbTMFrameQueue backend)) frames

storePUSPacket :: (MonadIO m) => DbBackend -> ExtractedDU PUSPacket -> m ()
storePUSPacket backend pkt = do
    atomically $ writeTBQueue (_dbbPUSPacketQueue backend) pkt


storeTMPacket :: (MonadIO m) => DbBackend -> TMPacket -> m ()
storeTMPacket backend pkt = do
    atomically $ writeTBQueue (_dbbTMPacketQueue backend) pkt

stopDbProcessing
    :: (MonadIO m, MonadReader env m, HasLogFunc env) => DbBackend -> m ()
stopDbProcessing backend = do
    logWarn "Stopping database backend..."
    cancel (_dbbThread backend)
    logWarn "Stopping database backend..."

