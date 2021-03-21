{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}
{-# LANGUAGE FlexibleContexts #-}
module Main (main) where

import           RIO
import           Test.Hspec
-- import           Database.Selda
-- import           Database.Selda.SQLite           ( withSQLite )

import           Persistence.Logging


data TestApp = TestApp { logFunc :: !LogFunc }

instance HasLogFunc TestApp where
  logFuncL = lens logFunc (const TestApp)


main :: IO ()
main = hspec $ do
  return ()
  -- describe "Logging to database" $ do
  --   let dbName = "file:test_logging?mode=memory&cache=shared"
  --   let runTest logLvl app = withSQLite dbName $ do
  --         liftIO $ withDatabaseLogger dbName logLvl
  --           $ \lf -> runRIO (TestApp lf) (void app)
  --         query $ do
  --           ev <- select (table' :: Table LogEvent)
  --           order (ev ! #logMessage) ascending
  --           return (ev ! #logLevel :*: ev ! #logMessage)

  --   it "just works" $ do
  --     res <- liftIO $ runTest LevelDebug $ do
  --       logInfo  "1. hello" :: RIO TestApp ()
  --       logDebug "2. from"
  --       logWarn  "3. other"
  --       logError "4. planets"

  --     res `shouldBe`
  --       [ "Info"  :*: "1. hello"
  --       , "Debug" :*: "2. from"
  --       , "Warn"  :*: "3. other"
  --       , "Error" :*: "4. planets"
  --       ]

  --   it "filters records by log level " $ do
  --     res <- liftIO $ runTest LevelWarn $ do
  --       logWarn  "1. hello" :: RIO TestApp ()
  --       logDebug "2. from"
  --       logWarn  "3. other"
  --       logError "4. planets"

  --     res `shouldBe`
  --       [ "Warn"  :*: "1. hello"
  --       , "Warn"  :*: "3. other"
  --       , "Error" :*: "4. planets"
  --       ]

-- TEST: unable to open database file
-- TEST: invalid table
-- TEST: with / without delay
