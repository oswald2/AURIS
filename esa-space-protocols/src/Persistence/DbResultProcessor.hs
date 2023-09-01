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
    raiseEvent (EVDB (EVDBTMFrames frames))
dbResultFunc DBResultTMFramesFinished = do 
    logDebug $ "Query for TM Frames finished."
    raiseEvent (EVDB EVDBTMFramesFinished)

dbResultFunc (DBResultTMPackets packets)  = do
    logDebug $ "Query Result: " <> display (length packets) <> " rows."
    raiseEvent (EVDB (EVDBTMPackets packets))
dbResultFunc DBResultTMPacketsFinished = do 
    logDebug $ "Query for TM Packets finished."
    raiseEvent (EVDB EVDBTMPacketsFinished)

dbResultFunc _ = return ()