module Persistence.DbResultProcessor
    ( dbResultFunc
    ) where

import           RIO

import           Control.PUS.Classes

import           Persistence.DBQuery

import           Data.PUS.Events


dbResultFunc
    :: (MonadIO m, MonadReader env m, HasLogFunc env, HasRaiseEvent env)
    => DBResult
    -> m ()
dbResultFunc (DBResultTMFrames frames)  = do
    logDebug $ "Query Result: " <> display (length frames) <> " rows."
    env <- ask
    liftIO $ raiseEvent env (EVDB (EVDBTMFrames frames))
dbResultFunc _ = return ()