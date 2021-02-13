{-# OPTIONS_GHC -fno-warn-orphans #-}
module Persistence.Conversion.LogEvent where

import           RIO

import           Persistence.LogEvent
import           Persistence.Definitions
import           Persistence.Conversion.Types   ( DbConversion(..) )




instance DbConversion LogEvent DbLogEvent where 
  toDB LogEvent {..} = DbLogEvent {
        dbLogEventTimestamp = logEventTimestamp 
        , dbLogEventSource = logEventSource 
        , dbLogEventLevel = levelToDB logEventLevel 
        , dbLogEventMessage = utf8BuilderToText logEventMessage 
      }
  fromDB DbLogEvent {..} = LogEvent {
        logEventTimestamp = dbLogEventTimestamp 
        , logEventSource = dbLogEventSource 
        , logEventLevel = levelFromDB dbLogEventLevel 
        , logEventMessage = display dbLogEventMessage
      }


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