module Persistence.LogEvent
    ( LogEvent(..)
    ) where

import           RIO                            ( Utf8Builder
                                                , LogLevel
                                                , Text
                                                )

import           Data.Time.Clock                ( UTCTime )


data LogEvent = LogEvent
    { logEventTimestamp :: !UTCTime
    , logEventSource    :: !Text
    , logEventLevel     :: LogLevel
    , logEventMessage   :: Utf8Builder
    }



