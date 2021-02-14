module Data.Mongo.Processing
    ( Data.Mongo.Processing.connect
    , startDbProcessing
    , withMongoDB
    ) where

import           RIO
import qualified RIO.Text                      as T
import qualified Data.Text.IO                  as T
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


connect :: DbConfigMongoDB -> IO (Either Text Pipe)
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
            res <- E.try $ DB.connect conn
            case res of
                Left e ->
                    return
                        $ Left
                              (  "Error on connecting to MongoDB: "
                              <> T.pack (show (e :: IOError))
                              )
                Right c -> return $ Right c


startDbProcessing :: DbConfigMongoDB -> IO DbBackend
startDbProcessing cfg = do
    frameQueue <- newTBQueueIO dbQueueSize
    eventQueue <- newTBQueueIO dbQueueSize
    pktQueue   <- newTBQueueIO dbQueueSize

    let tmFramesT = conc $ tmFrameThread cfg frameQueue
        eventsT   = conc $ eventStoreThread cfg eventQueue
        pktsT     = conc $ pusPacketThread cfg pktQueue

        threads   = tmFramesT <> eventsT <> pktsT

    t <- async $ runConc threads

    createDbBackend frameQueue eventQueue pktQueue t (getAllFrames cfg)


portParser :: Parser PortID
portParser = do
    A.try (PortNumber <$> decimal) <|> UnixSocket . T.unpack <$> takeText


withMongoDB
    :: DbConfigMongoDB
    -> (Either Text Pipe -> IO (Either Text ()))
    -> IO (Either Text ())
withMongoDB cfg action = do
    bracket (Data.Mongo.Processing.connect cfg) shutdown action

  where
    shutdown (Left  err) = return (Left err)
    shutdown (Right _  ) = return (Right ())


runInConnection
    :: DbConfigMongoDB -> (DbConfigMongoDB -> Pipe -> IO ()) -> IO ()
runInConnection cfg worker = do
    bracket
        (Data.Mongo.Processing.connect cfg)
        (\case
            Left  _    -> return ()
            Right pipe -> DB.close pipe
        )
        (process worker)
  where
    process _ (Left err) = do
        T.putStrLn
            $  "Could not connect to MongoDB: "
            <> err
            <> "\nConfig: "
            <> T.pack (show cfg)
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


tmFrameThread :: DbConfigMongoDB -> TBQueue TMStoreFrame -> IO ()
tmFrameThread cfg queue = do
    runInConnection cfg worker
  where
    worker _ pipe = forever $ do
        frames <- atomically $ flushTBQueue queue
        writeDB pipe "tm_frames" (map toDB frames)

eventStoreThread :: DbConfigMongoDB -> TBQueue LogEvent -> IO ()
eventStoreThread cfg queue = do
    runInConnection cfg worker
  where
    worker _ pipe = forever $ do
        logs <- atomically $ flushTBQueue queue
        writeDB pipe "log_events" (map toDB logs)


pusPacketThread :: DbConfigMongoDB -> TBQueue (ExtractedDU PUSPacket) -> IO ()
pusPacketThread cfg queue = do
    runInConnection cfg worker
  where
    worker _ pipe = forever $ do
        pkts <- atomically $ flushTBQueue queue
        writeDB pipe "pus_packets" (map toDB pkts)


getAllFrames :: DbConfigMongoDB -> IO [TMStoreFrame]
getAllFrames cfg = do
    res <- Data.Mongo.Processing.connect cfg
    case res of
        Left  _    -> return []
        Right pipe -> do
            frames <- worker pipe
            DB.close pipe
            return frames

  where
    worker pipe = access pipe master "active_session" query
    query = catMaybes . map fromDB <$> (find (select [] "tm_frames") >>= rest)
