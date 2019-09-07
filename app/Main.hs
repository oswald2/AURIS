
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Db

import           EventLog

import           RIO

import           System.Random

data Logging a = Logging { appLogFunc :: LogFunc, applicationCtx :: a }

instance HasLogFunc (Logging a) where
    logFuncL :: Lens' (Logging a) LogFunc
    logFuncL d (Logging x a) = (\x -> Logging x a) <$> d x

testApp :: RIO (Logging ()) ()
testApp = do
    logInfo "Starting 1"
    logWarn "Starting 1"
    logError "Starting 1"

main = do
    logOptions <- logOptionsHandle stderr True
    (termLog, killTL) <- newLogFunc logOptions
    (logDB, killDB) <- logToSQLiteDatabase "test.db"
    runRIO (Logging termLog ()) $ prependLogger logDB testApp
    killDB
    killTL

