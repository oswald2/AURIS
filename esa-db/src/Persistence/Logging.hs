{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}

module Persistence.Logging
  ( Table' (..)
  , LogEvent (..)
  , withDatabaseLogger
  )
where

import           RIO
import           Data.Time.Clock ( UTCTime, getCurrentTime )
import           Database.Selda
import           Persistence.Internals


-- | Wraps monadic action and provides a RIO-compatible log function.
withDatabaseLogger
  :: (MonadUnliftIO m, MonadMask m)
  => FilePath -> LogLevel -> (LogFunc -> m b) -> m b
withDatabaseLogger dbPath minLvl app = withDatabaseWriter dbPath app'
  where
    app' writeFn = do
      -- Convert RIO's log info into database-writable LogEvent.
      let logFn = mkLogFunc $ \_stack _src lvl msg ->
            when (lvl >= minLvl) $ do
              now <- getCurrentTime
              writeFn $ LogEvent now (logLevelToText lvl) (textDisplay msg)
      app logFn


data LogEvent = LogEvent {
  logTime      :: !UTCTime
  , logLevel   :: !Text
  , logMessage :: !Text
  }
  deriving (Show, Generic)

instance SqlRow LogEvent

instance Table' LogEvent where
  table' = table "log_events"
    [#logTime :- indexUsing BTreeIndex]


logLevelToText :: LogLevel -> Text
logLevelToText = \case
  LevelDebug   -> "Debug"
  LevelInfo    -> "Info"
  LevelWarn    -> "Warn"
  LevelError   -> "Error"
  LevelOther t -> t
