-- |
-- Module      :  EventLog
-- Copyright   :  Paolo Veronelli, Matthias Putz
-- License     :  BSD3
--
-- Maintainer  :  paolo@global.de
-- Stability   :  experimental
-- Portability :  unknown
--
-- Support RIO external logging
--

{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module EventLog where

import RIO
import Data.Text
import Data.Time


data EventLog = EventLog
    { logTime    :: UTCTime -- ^  timestamp for the event
    , logLevel   :: LogLevel -- ^ severity 
    , logMessage :: Text -- ^ message
    }
  deriving (Generic)

type EventLogger = EventLog -> IO ()

mkLoggerF :: EventLogger -> LogFunc
mkLoggerF log = mkLogFunc $ \_ _ ll t -> do
    time <- getCurrentTime
    log $ EventLog time ll $ textDisplay t

prependLogger :: HasLogFunc e => EventLogger -> RIO e b -> RIO e b
prependLogger log = local (over logFuncL $ mappend $ mkLoggerF log) 

