module Persistence.Logging
    ( logToDB
    ) where

import           RIO

import           Data.Time.Clock

import           Persistence.DbBackend
import           Persistence.LogEvent


logToDB
    :: (MonadIO m) => DbBackend -> CallStack -> LogSource -> LogLevel -> Utf8Builder -> m ()
logToDB _       _ _      LevelDebug _       = return ()
logToDB backend _ source level      builder = do
    now <- liftIO $ getCurrentTime
    let entry = LogEvent { logEventTimestamp = now
                           , logEventSource    = source
                           , logEventLevel     = level
                           , logEventMessage   = builder
                           }
    storeLog backend entry



