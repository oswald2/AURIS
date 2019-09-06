{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, InstanceSigs #-}

module DbLogging where


import           Data.Text as T
import           Data.Time
import           RIO
import EventLog

mkLoggerF log = mkLogFunc $ \_ _ ll t -> do
            time <- getCurrentTime
            log $ EventLog time ll $ textDisplay t

prependLogger :: HasLogFunc e => EventLogger -> RIO e b -> RIO e b
prependLogger log = local (over logFuncL $ mappend $ mkLoggerF log) 

