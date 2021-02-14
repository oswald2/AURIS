{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Mongo.Processing
    ( Data.Mongo.Processing.connect
    , newDbState
    , DbState
    , startDbProcessing
    ) where

import           RIO
import qualified RIO.Text                      as T
import qualified RIO.List                      as L
import           Prelude                        ( IOError )
import           Control.Concurrent.STM.TBQueue ( flushTBQueue )

import           Data.Attoparsec.Text          as A
                                                ( try
                                                , parseOnly
                                                , takeText
                                                , Parser
                                                , decimal
                                                )

import           UnliftIO.Exception            as E
import           Database.MongoDB              as DB

import           Data.PUS.ExtractedDU
import           Data.PUS.PUSPacket

import           Data.DbConfig.MongoDB
import           Data.Mongo.Conversion.Class
import           Data.Mongo.Conversion.TMFrame  ( )
import           Data.Mongo.Conversion.LogEvent ( )
import           Data.Mongo.Conversion.PUSPacket
                                                ( )
import           Data.Mongo.Conversion.ExtractedDU
                                                ( )
import           Data.PUS.TMStoreFrame

import           Persistence.DbBackend
import           Persistence.LogEvent



dbQueueSize :: Natural
dbQueueSize = 10000


instance DbBackendClass DbConfigMongoDB m where
    allTMFrames       = getAllFrames
    dropTMFramesTable = cleanFramesTable


connect :: (MonadUnliftIO m) => DbConfigMongoDB -> m (Either Text Pipe)
connect cfg = do
    let h    = T.unpack (cfgMongoHost cfg)
        port = case cfgMongoPort cfg of
            Nothing -> Right defaultPort
            Just x  -> case parseOnly portParser x of
                Left err -> Left
                    ("Cannot parse MongoDB port identifier: " <> T.pack err)
                Right p -> Right p

    case port of
        Left  err -> return (Left err)
        Right p   -> do
            let conn = Host h p
            res <- E.try $ liftIO $ DB.connect conn
            case res of
                Left e ->
                    return
                        $ Left
                              (  "Error on connecting to MongoDB: "
                              <> T.pack (show (e :: IOError))
                              )
                Right c -> return $ Right c


newtype DbState = DbState
    { dbStLogFunc :: LogFunc
    }

newDbState :: LogFunc -> IO DbState
newDbState logFunc = return DbState { dbStLogFunc = logFunc }

instance HasLogFunc DbState where
    logFuncL = lens dbStLogFunc (\c lf -> c { dbStLogFunc = lf })



startDbProcessing
    :: (MonadUnliftIO m, MonadReader env m, HasLogFunc env)
    => DbConfigMongoDB
    -> m DbBackend
startDbProcessing cfg = do
    frameQueue <- liftIO $ newTBQueueIO dbQueueSize
    eventQueue <- liftIO $ newTBQueueIO dbQueueSize
    pktQueue   <- liftIO $ newTBQueueIO dbQueueSize

    let tmFramesT = conc $ tmFrameThread cfg frameQueue
        eventsT   = conc $ eventStoreThread cfg eventQueue
        pktsT     = conc $ pusPacketThread cfg pktQueue

        threads   = tmFramesT <> eventsT <> pktsT

    t <- async $ runConc threads

    initDB cfg

    createDbBackend frameQueue eventQueue pktQueue t



initDB
    :: (MonadReader env m, MonadUnliftIO m, HasLogFunc env)
    => DbConfigMongoDB
    -> m ()
initDB cfg = do
    runInConnection cfg worker
  where
    worker _ pipe = access pipe master "active_session" query
    query = do
        -- create index on "tm_frames" collection
        let idx = Index { iColl               = "tm_frames"
                        , iKey                = ["ert" =: (1 :: Int)]
                        , iName               = "TMFrameIndex"
                        , iUnique             = True
                        , iDropDups           = False
                        , iExpireAfterSeconds = Nothing
                        }
        ensureIndex idx

        -- create index on "log_events" collection
        let logIdx = Index { iColl               = "log_events"
                           , iKey                = ["timestamp" =: (1 :: Int)]
                           , iName               = "LogEventIndex"
                           , iUnique             = True
                           , iDropDups           = False
                           , iExpireAfterSeconds = Nothing
                           }
        ensureIndex logIdx

        -- create index on "log_events" collection
        let pktIdx = Index { iColl               = "pus_packets"
                           , iKey                = ["ert" =: (1 :: Int), "dfh.secHeader.time" =: (1 :: Int)]
                           , iName               = "PUSPacketIndex"
                           , iUnique             = True
                           , iDropDups           = False
                           , iExpireAfterSeconds = Nothing
                           }
        ensureIndex pktIdx



portParser :: Parser PortID
portParser = do
    A.try (PortNumber <$> decimal) <|> UnixSocket . T.unpack <$> takeText



runInConnection
    :: (MonadUnliftIO m, MonadReader env m, HasLogFunc env)
    => DbConfigMongoDB
    -> (DbConfigMongoDB -> Pipe -> m ())
    -> m ()
runInConnection cfg worker = do
    e <- E.try $ bracket
        (Data.Mongo.Processing.connect cfg)
        (\case
            Left  _    -> return ()
            Right pipe -> liftIO $ DB.close pipe
        )
        (process worker)
    case e of
        Left err ->
            logError $ "Exception during database processing: " <> displayShow
                (err :: SomeException)
        Right _ -> return ()
  where
    process _ (Left err) = do
        logError
            $  "Could not connect to MongoDB: "
            <> display err
            <> "\nConfig: "
            <> displayShow cfg
    process worker' (Right pipe) = do
        worker' cfg pipe


writeDB :: MonadIO m => Pipe -> Collection -> [Document] -> m ()
writeDB pipe collection dat = access pipe master "active_session" (go dat)
  where
    go []     = return ()
    go frames = do
        let (beg, r) = L.splitAt 1000 frames
        insertAll_ collection beg
        go r


tmFrameThread
    :: (MonadUnliftIO m, MonadReader env m, HasLogFunc env)
    => DbConfigMongoDB
    -> TBQueue TMStoreFrame
    -> m ()
tmFrameThread cfg queue = do
    runInConnection cfg worker
  where
    worker _ pipe = do
        logDebug "TM Frame Store Thread started"
        forever $ do
            logDebug "tmFrameThread waiting for frames..."
            frames <- atomically $ do
                f  <- readTBQueue queue
                fs <- flushTBQueue queue
                return (f : fs)
            liftIO $ writeDB pipe "tm_frames" (map toDB frames)
            logInfo ("Stored " <> display (length frames) <> " TM Frames.")

eventStoreThread
    :: (MonadUnliftIO m, MonadReader env m, HasLogFunc env)
    => DbConfigMongoDB
    -> TBQueue LogEvent
    -> m ()
eventStoreThread cfg queue = do
    runInConnection cfg worker
  where
    worker _ pipe = do
        logDebug "Log Event Store Thread started"
        forever $ do
            logs <- atomically $ do
                e  <- readTBQueue queue
                es <- flushTBQueue queue
                return (e : es)
            liftIO $ writeDB pipe "log_events" (map toDB logs)
            logDebug $ "Stored " <> display (length logs) <> " Log Events."


pusPacketThread
    :: (MonadUnliftIO m, MonadReader env m, HasLogFunc env)
    => DbConfigMongoDB
    -> TBQueue (ExtractedDU PUSPacket)
    -> m ()
pusPacketThread cfg queue = do
    runInConnection cfg worker
  where
    worker _ pipe = do
        logDebug "PUS Packet Store Thread started"
        forever $ do
            pkts <- atomically $ do
                p  <- readTBQueue queue
                ps <- flushTBQueue queue
                return (p : ps)
            liftIO $ writeDB pipe "pus_packets" (map toDB pkts)
            logDebug $ "Stored " <> display (length pkts) <> " PUS Packets."




getAllFrames
    :: (MonadUnliftIO m, MonadReader env m, HasLogFunc env)
    => DbConfigMongoDB
    -> m [TMStoreFrame]
getAllFrames cfg = do
    res <- Data.Mongo.Processing.connect cfg
    case res of
        Left err -> do
            logWarn $ "Could not connect to DB: " <> display err
            return []
        Right pipe -> do
            frames <- liftIO $ worker pipe
            liftIO $ DB.close pipe
            return frames

  where
    worker pipe = access pipe master "active_session" query
    query = catMaybes . map fromDB <$> (find (select [] "tm_frames") >>= rest)


cleanFramesTable
    :: (MonadUnliftIO m, MonadReader env m, HasLogFunc env)
    => DbConfigMongoDB
    -> m ()
cleanFramesTable cfg = do
    res <- Data.Mongo.Processing.connect cfg
    case res of
        Left err -> do
            logWarn $ "Could not connect to DB: " <> display err
            return ()
        Right pipe -> do
            logDebug "Removing TM Frames table..."
            frames <- liftIO $ worker pipe
            liftIO $ DB.close pipe
            return frames
  where
    worker pipe = access pipe master "active_session" query
    query = void $ dropCollection "tm_frames"
