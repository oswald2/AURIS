module Persistence.Logging
    ( logToDB
    , levelToDB
    , levelFromDB
    ) where

import           RIO

import           Data.Time.Clock

import           Persistence.Definitions
import           Persistence.DbProcessing


logToDB
    :: DbBackend -> CallStack -> LogSource -> LogLevel -> Utf8Builder -> IO ()
logToDB _       _ _      LevelDebug _       = return ()
logToDB backend _ source level      builder = do
    now <- getCurrentTime
    let entry = DbLogEvent { dbLogEventTimestamp = now
                           , dbLogEventSource    = source
                           , dbLogEventLevel     = levelToDB level
                           , dbLogEventMessage   = utf8BuilderToText builder
                           }
    storeLog backend entry



levelToDB :: LogLevel -> Word8
levelToDB LevelDebug     = 0
levelToDB LevelInfo      = 1
levelToDB LevelWarn      = 2
levelToDB LevelError     = 3
levelToDB (LevelOther _) = 4

levelFromDB :: Word8 -> LogLevel
levelFromDB 0 = LevelDebug 
levelFromDB 1 = LevelInfo 
levelFromDB 2 = LevelWarn 
levelFromDB 3 = LevelError 
levelFromDB _ = LevelOther "" 




