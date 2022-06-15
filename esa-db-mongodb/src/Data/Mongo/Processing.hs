{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Mongo.Processing
    ( Data.Mongo.Processing.connect
    , newDbState
    , DbState
    , startDbStoreThreads
    , startDbQueryThreads
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
                                                ( try )
import           Database.MongoDB              as DB

import           Data.PUS.ExtractedDU           ( ExtractedDU )
import           Data.PUS.PUSPacket             ( PUSPacket )
import           Data.PUS.TMPacket              ( TMPacket )

import           Data.DbConfig.MongoDB
import           Data.Mongo.Conversion.Class    ( MongoDbConversion
                                                    ( fromDB
                                                    , toDB
                                                    )
                                                )
import           Data.Mongo.Conversion.TMFrame  ( )
import           Data.Mongo.Conversion.LogEvent ( )
import           Data.Mongo.Conversion.PUSPacket
                                                ( )
import           Data.Mongo.Conversion.TMPacket ( )
import           Data.Mongo.Conversion.ExtractedDU
                                                ( )
import           Data.PUS.TMFrame

import           Persistence.DbBackend
import           Persistence.LogEvent           ( LogEvent )
import           Persistence.DBQuery

import           General.Time



dbQueueSize :: Natural
dbQueueSize = 10000

dbName :: Text
dbName = "active_session"

tmFrameCollName :: Collection
tmFrameCollName = "tm_frames"

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


-- | This function starts threads in the background, which stream incoming data
-- to the database. As this creates the 'DbBackend', this has to be run before
-- the full processing of esa-space-protocols as the DbBackend is passed in to the 
-- RIO state. This means, that the RIO functionality would not be available to this 
-- functions (e.g. logging). Therefore, this functions is intended to be started with 
-- the 'DbState' in the RIO monad, and the logging function will be the main one used 
-- from esa-space-protocols (provided by 'withLogFunc'). 
--
-- As a second step, the query thread loop has to be run within the normal RIO processing
-- from esa-space-protocols, so the startup is a two-step process, first calling this 
-- function from it's own RIO context, then create the RIO context of the main processing
-- and then calling the 'startDbQueryThreads' within the RIO context. 
startDbStoreThreads
    :: (MonadUnliftIO m, MonadReader env m, HasLogFunc env)
    => DbConfigMongoDB
    -> m DbBackend
startDbStoreThreads cfg = do
    frameQueue <- liftIO $ newTBQueueIO dbQueueSize
    eventQueue <- liftIO $ newTBQueueIO dbQueueSize
    pktQueue   <- liftIO $ newTBQueueIO dbQueueSize
    tmPktQueue <- liftIO $ newTBQueueIO dbQueueSize

    let tmFramesT = conc $ tmFrameThread cfg frameQueue
        eventsT   = conc $ eventStoreThread cfg eventQueue
        pktsT     = conc $ pusPacketThread cfg pktQueue
        tmPktsT   = conc $ tmPacketThread cfg tmPktQueue

        threads   = tmFramesT <> eventsT <> pktsT <> tmPktsT

    t <- async $ runConc threads

    initDB cfg

    createDbBackend frameQueue eventQueue pktQueue tmPktQueue t



initDB
    :: (MonadReader env m, MonadUnliftIO m, HasLogFunc env)
    => DbConfigMongoDB
    -> m ()
initDB cfg = do
    runInConnection cfg worker
  where
    worker _ pipe = access pipe master dbName query
    query = do
        -- create index on tmFrameCollName collection
        let idx = Index { iColl               = tmFrameCollName
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

        -- create index on "pus_packets" collection
        let pktIdx = Index
                { iColl               = "pus_packets"
                , iKey                = [ "ert" =: (1 :: Int)
                                        , "dfh.secHeader.time" =: (1 :: Int)
                                        ]
                , iName               = "PUSPacketIndex"
                , iUnique             = True
                , iDropDups           = False
                , iExpireAfterSeconds = Nothing
                }
        ensureIndex pktIdx

        -- create index on "tm_packets" collection
        let tmPktIdx = Index { iColl               = "tm_packets"
                             , iKey                = ["timestamp" =: (1 :: Int)]
                             , iName               = "TMPacketIndex"
                             , iUnique             = True
                             , iDropDups           = False
                             , iExpireAfterSeconds = Nothing
                             }
        ensureIndex tmPktIdx



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
writeDB pipe collection dat = access pipe master dbName (go dat)
  where
    go []     = return ()
    go frames = do
        let (beg, r) = L.splitAt 1000 frames
        insertAll_ collection beg
        go r


tmFrameThread
    :: (MonadUnliftIO m, MonadReader env m, HasLogFunc env)
    => DbConfigMongoDB
    -> TBQueue (ExtractedDU TMFrame)
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
            liftIO $ writeDB pipe tmFrameCollName (map toDB frames)
            logDebug ("Stored " <> display (length frames) <> " TM Frames.")

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


tmPacketThread
    :: (MonadUnliftIO m, MonadReader env m, HasLogFunc env)
    => DbConfigMongoDB
    -> TBQueue TMPacket
    -> m ()
tmPacketThread cfg queue = do
    runInConnection cfg worker
  where
    worker _ pipe = do
        logDebug "TM Packet Store Thread started"
        forever $ do
            pkts <- atomically $ do
                p  <- readTBQueue queue
                ps <- flushTBQueue queue
                return (p : ps)
            liftIO $ writeDB pipe "tm_packets" (map toDB pkts)
            logDebug $ "Stored " <> display (length pkts) <> " TM Packets."



getAllFrames
    :: (MonadUnliftIO m, MonadReader env m, HasLogFunc env)
    => DbConfigMongoDB
    -> m [ExtractedDU TMFrame]
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
    worker pipe = access pipe master dbName query
    query =
        catMaybes . map fromDB <$> (find (select [] tmFrameCollName) >>= rest)

-- | Start the query thread. This thread needs the config, a backend, the result function
-- (which will probably be mapped to the raiseEvent function within the processing) and 
-- a queue receiving the queries to execute.
startDbQueryThreads
    :: (MonadUnliftIO m, MonadReader env m, HasLogFunc env)
    => DbConfigMongoDB
    -> DbBackend
    -> (DBResult -> m ())
    -> TBQueue DBQuery
    -> m ()
startDbQueryThreads cfg backend resultHandler queryQueue = do
    queryT <- async $ queryProcessor cfg resultHandler queryQueue
    rememberQueryThread backend queryT

queryProcessor
    :: (MonadUnliftIO m, MonadReader env m, HasLogFunc env)
    => DbConfigMongoDB
    -> (DBResult -> m ())
    -> TBQueue DBQuery
    -> m ()
queryProcessor cfg resultHandler queryQueue = do
    res <- Data.Mongo.Processing.connect cfg
    case res of
        Left err -> do
            logWarn $ "Could not connect to DB: " <> display err
        Right pipe -> forever $ do
            query <- atomically $ readTBQueue queryQueue
            queryHandler resultHandler pipe query


queryHandler
    :: (MonadUnliftIO m, MonadReader env m, HasLogFunc env)
    => (DBResult -> m ())
    -> Pipe
    -> DBQuery
    -> m ()
queryHandler resultF pipe (FrRange q)
    = do
        logDebug $ "QueryHandler: query=" <> displayShow q
        let start1 = dbFromTime q
            stop1 = dbToTime q
        case (start1, stop1) of 
            (Nothing, Nothing) -> return () 
            (Just start, Just stop) -> frameFromTo resultF pipe start stop
            (Just start, Nothing) -> frameFrom resultF pipe start 
            (Nothing, Just stop) -> frameTo resultF pipe stop 
queryHandler resultF pipe (FrPrev q) = do 
    logDebug $ "QueryPrev: " <> displayShow q 
    frameToN resultF pipe (dbStart q) (dbN q)
queryHandler resultF pipe (FrNext q) = do 
    logDebug $ "QueryPrev: " <> displayShow q 
    frameFromN resultF pipe (dbnStart q) (dbnN q)

resultSize :: BatchSize
resultSize = 1000


collectQuery :: (MonadIO m, MongoDbConversion b Document) 
    => (DBResult -> m ())
    -> Pipe 
    -> Query 
    -> ([b] -> DBResult)
    -> DBResult
    -> m ()
collectQuery resultF pipe query constr finishConstr = do 
    access pipe master dbName $ do 
        cursor <- find query 
        -- we send the results in batches of size @resultSize@
        let sendF = do 
                docs <- nextBatch cursor 
                let records = mapMaybe fromDB docs 

                -- traceM $ "Docs returned: " <> textDisplay (length docs)
                -- traceM $ "Records returned: " <> textDisplay (length records)
                if null records 
                    then lift $ resultF $ finishConstr 
                    else do 
                        lift $ resultF $ constr records 
                        sendF
        -- execute the function
        sendF 
        closeCursor cursor


frameFromTo
    :: (MonadIO m) => (DBResult -> m ()) -> Pipe -> SunTime -> SunTime -> m ()
frameFromTo resultF pipe start stop = do
    let query = (select ["ert" =: ["$gte" =: timeToMicro start, "$lte" =: timeToMicro stop]]
                    tmFrameCollName) { sort = ["ert" =: Int32 (-1)], batchSize = resultSize }

    -- traceM $ utf8BuilderToText $ "Query: " <> displayShow query
    collectQuery resultF pipe query DBResultTMFrames DBResultTMFramesFinished

frameFrom
    :: (MonadIO m) => (DBResult -> m ()) -> Pipe -> SunTime -> m ()
frameFrom resultF pipe start = do
    let query = (select ["ert" =: ["$gte" =: timeToMicro start]] 
                    tmFrameCollName) { sort = ["ert" =: Int32 (-1)], batchSize = resultSize }
    collectQuery resultF pipe query DBResultTMFrames DBResultTMFramesFinished

frameTo
    :: (MonadIO m) => (DBResult -> m ()) -> Pipe -> SunTime -> m ()
frameTo resultF pipe stop = do
    let query = (select ["ert" =: ["$lte" =: timeToMicro stop]]
                    tmFrameCollName) { sort = ["ert" =: Int32 (-1)], batchSize = resultSize }
    collectQuery resultF pipe query DBResultTMFrames DBResultTMFramesFinished




frameToN
    :: (MonadIO m) => (DBResult -> m ()) -> Pipe -> SunTime -> Word32 -> m ()
frameToN resultF pipe stop n = do
    let query = (select ["ert" =: ["$lte" =: timeToMicro stop]]
                    tmFrameCollName) { sort = ["ert" =: Int32 (-1)], limit = n }
    collectQuery resultF pipe query DBResultTMFrames DBResultTMFramesFinished

frameFromN
    :: (MonadIO m) => (DBResult -> m ()) -> Pipe -> SunTime -> Word32 -> m ()
frameFromN resultF pipe start n = do
    let query = (select ["ert" =: ["$gte" =: timeToMicro start]] 
                    tmFrameCollName) { sort = ["ert" =: Int32 (-1)], limit = n }
    collectQuery resultF pipe query DBResultTMFrames DBResultTMFramesFinished



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
    worker pipe = access pipe master dbName query
    query = void $ dropCollection tmFrameCollName
