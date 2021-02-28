module Persistence.DbResultProcessor
    ( dbResultFunc
    ) where

import           RIO

import           Control.PUS.Classes

import           Data.Mongo.DBQuery

import           Data.PUS.Events


dbResultFunc
    :: (MonadIO m, MonadReader env m, HasLogFunc env, HasRaiseEvent env)
    => DBResult
    -> m ()
dbResultFunc result = do
    logDebug $ "Result: " <> displayShow result
    env <- ask
    liftIO $ raiseEvent env (EVDB (EVDBTMFrames []))
