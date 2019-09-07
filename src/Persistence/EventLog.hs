-- |
-- Module      :  Persistence.EventLog
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

module Persistence.EventLog where

import           Control.Monad.Reader
import           Data.Text
import           Data.Time
import           RIO


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

prependLogger :: (MonadReader e m, HasLogFunc e) => EventLogger -> m b -> m b
prependLogger log = local (over logFuncL $ mappend $ mkLoggerF log) 

