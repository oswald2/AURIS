{-# LANGUAGE OverloadedStrings
    , BangPatterns
    , GeneralizedNewtypeDeriving
    , DeriveGeneric
    , RecordWildCards
    , FlexibleInstances
    , BinaryLiterals
#-}
module Main where


import           RIO
import qualified RIO.Text                      as T
import qualified Data.Text.IO                  as T

import           Data.PUS.GlobalState
import           Data.PUS.Config
import           Data.PUS.MissionSpecific.Definitions

import           Persistence.Db
import           Persistence.EventLog

import           GHC.Conc.Sync





main :: IO ()
main = do
  np <- getNumProcessors
  setNumCapabilities np

  (logDB, _killDB) <- logToSQLiteDatabase "test.db"

  defLogOptions <- logOptionsHandle stdout True
  let logOptions = setLogMinLevel LevelError defLogOptions
  withLogFunc logOptions $ \logFunc -> do
    state <- newGlobalState
      defaultConfig
      (defaultMissionSpecific defaultConfig)
      logFunc
      (\ev -> T.putStrLn ("Event: " <> T.pack (show ev)))

    runRIO state $ prependLogger logDB $ do
        logWarn "Warning"
        logError "Error"
        logInfo "Info"
        logDebug "Debug"

