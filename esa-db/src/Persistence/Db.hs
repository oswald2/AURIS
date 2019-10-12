-- |
-- Module      :  Persistence.Db
-- Copyright   :  Paolo Veronelli, Matthias Putz
-- License     :  BSD3
--
-- Maintainer  :  paolo@global.de
-- Stability   :  experimental
-- Portability :  unknown
--
-- Database backend for Events
--


{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Persistence.Db where

import           Control.Monad.State

import qualified Data.Text                       as T
--import           Data.Time

import           Database.Selda
import           Database.Selda.Backend
import           Database.Selda.Backend.Internal
import           Database.Selda.SQLite
--import           Database.Selda.SqlType

import           Persistence.EventLog

import           RIO

import           System.Directory

customText :: Text -> Lit a
customText = LCustom TText . LText

instance SqlType LogLevel where
    mkLit LevelDebug      = customText "debug"
    mkLit LevelInfo       = customText "info"
    mkLit LevelWarn       = customText "warn"
    mkLit LevelError      = customText "error"
    mkLit (LevelOther t)  = customText t
    sqlType _             = TText
    fromSql (SqlString x) = case x of
        "debug" -> LevelDebug
        "info"  -> LevelInfo
        "warn"  -> LevelWarn
        "error" -> LevelError
        y       -> LevelOther y
    defaultValue = customText "wtf"

instance SqlRow EventLog

eventLogTable :: Table EventLog
eventLogTable = tableFieldMod "events_log"
    [   #logTime :- index]
    (T.drop $ T.length "log")

runQ :: MonadIO m => MVar (SeldaConnection b) -> SeldaT b m a -> m ()
runQ cRef (S f) = do
    c <- takeMVar cRef
    (_, c') <- runStateT f c
    putMVar cRef c'

logToSQLiteDatabase :: FilePath -> IO (EventLogger, IO ())
logToSQLiteDatabase fp = do
    e <- doesFileExist fp
    cRef <- sqliteOpen fp >>= newMVar
    let run = runQ cRef
    when (not e) $ run $ createTable eventLogTable
    pure $ (run . insert_ eventLogTable . pure, readMVar cRef >>= seldaClose)

