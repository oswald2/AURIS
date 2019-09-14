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
import qualified Data.ByteString               as B
import qualified RIO.Text                      as T
import qualified Data.Text.IO                  as T

import           Conduit
import qualified Data.Conduit.List             as C
import           Data.Conduit.List
import           Data.Conduit.Network

import           Data.PUS.TCTransferFrame
import           Data.PUS.TCTransferFrameEncoder
import           Data.PUS.CLTU
import           Data.PUS.CLTUEncoder
import           Data.PUS.GlobalState
import           Data.PUS.Config
import           Data.PUS.Types
import           Data.PUS.PUSPacket
import           Data.PUS.PUSDfh
import           Data.PUS.PUSPacketEncoder
import           Data.PUS.SegmentEncoder
import           Data.PUS.SegmentationFlags
import           Data.PUS.APID
import           Data.PUS.TCRequest
import           Data.PUS.TCRequestEncoder
import           Data.PUS.TCPacketEncoder
import           Data.PUS.TCPacket
import           Data.PUS.Parameter
import           Data.PUS.Value
import           Data.PUS.MissionSpecific.Definitions

import           Persistence.Db
import           Persistence.EventLog

import           Protocol.NCTRS
import           Protocol.ProtocolInterfaces

import           General.ShowConduit

import           GHC.Conc.Sync





main :: IO ()
main = do
  np <- getNumProcessors
  setNumCapabilities np

  (logDB, killDB) <- logToSQLiteDatabase "test.db"

  defLogOptions <- logOptionsHandle stdout True
  let logOptions = setLogMinLevel LevelError defLogOptions
  withLogFunc logOptions $ \logFunc -> do
    state <- newGlobalState
      defaultConfig
      defaultMissionSpecific
      logFunc
      (\ev -> T.putStrLn ("Event: " <> T.pack (show ev)))

    runRIO state $ prependLogger logDB $ do
        logWarn "Warning"
        logError "Error"
        logInfo "Info"
        logDebug "Debug"

