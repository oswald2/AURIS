{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, InstanceSigs #-}

module DbLogging where


import           Data.Text as T
import           Data.Time
import           RIO
import EventLog


prependLogger :: EventLogger -> RIO (Logging a) b -> RIO (Logging a) b
prependLogger log app = do
    let lf = mkLogFunc $ \_ _ ll t -> do
            time <- getCurrentTime
            log $ EventLog time ll $ textDisplay t
    local (over logFuncL $ mappend lf) app

data Logging a = Logging
    { appLogFunc :: LogFunc
    , applicationCtx :: a
    }


instance HasLogFunc (Logging a) where
    logFuncL :: Lens' (Logging a) LogFunc
    logFuncL d (Logging x a) = (\x -> Logging x a) <$> d x

