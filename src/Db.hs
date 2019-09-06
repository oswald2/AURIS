{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Db
    ( EventLog(..)
    , logToDatabase,
    ) where

import           Control.Monad.State
import           Data.Text as T
import           Data.Time
import           RIO
import           Database.Selda
import           Database.Selda.Backend
import           Database.Selda.Backend.Internal
import           Database.Selda.SqlType


data EventLog = EventLog
    { utcTime :: UTCTime
    , logLevel :: LogLevel
    , logMessage :: T.Text
    } deriving (Generic)

instance SqlType LogLevel where
    mkLit LevelDebug = LCustom TText $ LText "debug"
    mkLit LevelInfo = LCustom TText $ LText "info"
    mkLit LevelWarn = LCustom TText $ LText "warn"
    mkLit LevelError = LCustom TText $ LText "error"
    mkLit (LevelOther t) = LCustom TText $ LText t

    sqlType _ = TText

    fromSql (SqlString x) = case x of
                              "debug" -> LevelDebug
                              "info" -> LevelInfo
                              "warn" -> LevelWarn
                              "error" -> LevelError
                              y -> LevelOther y

    defaultValue = LCustom TText $ LText "wtf"

instance SqlRow EventLog where

eventLogTable :: Table EventLog
eventLogTable = table "eventLog" []

logToDatabase :: EventLog -> ReaderT (IORef (SeldaConnection (Backend IO))) IO ()
logToDatabase e = do
    cRef <- ask
    c <- liftIO $ readIORef cRef
    (_, c') <- liftIO $ runStateT (unS f) c
    liftIO $ writeIORef cRef c'
  where
    f = insert_ eventLogTable [e]

