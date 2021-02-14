module Persistence.LogEvent
    ( LogEvent(..)
    ) where

import           RIO
import           Data.Time.Clock                ( UTCTime )


data LogEvent = LogEvent
    { logEventTimestamp :: !UTCTime
    , logEventSource   :: !Text
    , logEventLevel     :: LogLevel
    , logEventMessage   :: Utf8Builder
    }

instance Eq LogEvent where
    x1 == x2 =
        logEventTimestamp x1
            == logEventTimestamp x2 
        && logEventSource x1 == logEventSource x2 
        && logEventLevel x1 == logEventLevel x2 
        && utf8BuilderToText (logEventMessage x1)
            == utf8BuilderToText (logEventMessage x2)

instance Show LogEvent where
    show x = 
        "LogEvent { " <> "logEventTimestamp=" <> show (logEventTimestamp x)
        <> ", logEventSource=" <> show (logEventSource x)
        <> ", logEventLevel=" <> show (logEventLevel x)
        <> ", logEventMessage=" <> show (utf8BuilderToText (logEventMessage x))




