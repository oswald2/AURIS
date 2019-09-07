-- |
-- Module      :  Db
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
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Db where

import           Control.Monad.State

import           Data.Text                       as T
import           Data.Time

import           Database.Selda
import           Database.Selda.Backend
import           Database.Selda.Backend.Internal
import           Database.Selda.SQLite
import           Database.Selda.SqlType

import           EventLog

import           RIO


instance SqlType LogLevel where
    mkLit LevelDebug      = LCustom TText $ LText "debug"
    mkLit LevelInfo       = LCustom TText $ LText "info"
    mkLit LevelWarn       = LCustom TText $ LText "warn"
    mkLit LevelError      = LCustom TText $ LText "error"
    mkLit (LevelOther t)  = LCustom TText $ LText t
    sqlType _             = TText
    fromSql (SqlString x) = case x of
        "debug" -> LevelDebug
        "info"  -> LevelInfo
        "warn"  -> LevelWarn
        "error" -> LevelError
        y       -> LevelOther y
    defaultValue = LCustom TText $ LText "wtf"

instance SqlRow EventLog 

eventLogTable :: Table EventLog
eventLogTable = table "eventLog" []

runQ :: MonadIO m => IORef (SeldaConnection b) -> SeldaT b m a -> m ()
runQ cRef (S f) = do
    c <- readIORef cRef
    (_, c') <- runStateT f c
    writeIORef cRef c'

logToSQLiteDatabase :: FilePath -> IO (EventLogger, IO ())
logToSQLiteDatabase fp = do
    cRef <- sqliteOpen fp >>= newIORef
    let run = runQ cRef
    run $ tryCreateTable eventLogTable
    pure $ (run . insert_ eventLogTable . pure, readIORef cRef >>= seldaClose)

