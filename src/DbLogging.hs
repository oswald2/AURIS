{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DbLogging
    ( HasLog(..)
    , runWithLogs
    , test
    ) where


import           Data.Text as T
import           Data.Time
import           RIO
import           Database.Selda
import           Database.Selda.Backend

import           Db


runWithLogs :: (EventLog -> ReaderT (IORef c) IO ()) -> RIO (HasLog ctx c) b -> RIO (HasLog ctx c) b
runWithLogs logToDatabase app = do
    ctx <- ask
    let lf = mkLogFunc $ \_ _ ll t -> do
        time <- getCurrentTime
        runReaderT
            do logToDatabase $ EventLog time ll (textDisplay t)
            do databaseConnection ctx
    local (over logFuncL $ mappend lf ) app

data HasLog a c = HasLog
    { appLogFunc :: LogFunc
    , databaseConnection :: IORef c
    , application :: a
    }

-- appLens :: Functor f => (LogFunc -> f LogFunc) -> App -> f App
appLens :: Lens' (HasLog a c) LogFunc
appLens d (HasLog x c a) = (\x -> HasLog x c a) <$> d x

instance HasLogFunc (HasLog a c) where
    logFuncL = appLens

myApp :: RIO (HasLog () c) ()
myApp = do
    logInfo "Starting 1"
    logWarn "Starting 1"
    logError "Starting 1"
    pure ()

test = runWithLogs (logToDatabase) myApp
