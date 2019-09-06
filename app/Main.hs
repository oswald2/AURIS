
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           RIO
import           System.Random

import           DbLogging
import Db

testApp :: RIO (Logging ()) ()
testApp = do
    logInfo "Starting 1"
    logWarn "Starting 1"
    logError "Starting 1"

main = do
    logOptions <- logOptionsHandle stderr True 
    (termLog, killTL) <- newLogFunc logOptions
    (log, killDB) <- logToSQLiteDatabase "test.db"
    runRIO (Logging termLog ()) $ runWithLogs log testApp
    killDB
    killTL

