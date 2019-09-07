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

import           Conduit
import           Data.Conduit.Network

import           Data.PUS.GlobalState
import           Data.PUS.Config
import           Data.PUS.MissionSpecific.Definitions
import           Data.PUS.TMFrameExtractor
import           Data.PUS.NcduToTMFrame
import           Protocol.NCTRS
import           Protocol.ProtocolInterfaces

import           GHC.Conc.Sync

import           General.ShowConduit




main :: IO ()
main = do
  np <- getNumProcessors
  setNumCapabilities np

  defLogOptions <- logOptionsHandle stdout True
  let logOptions = setLogMinLevel LevelError defLogOptions
  withLogFunc logOptions $ \logFunc -> do
    state <- newGlobalState
      defaultConfig
      defaultMissionSpecific
      logFunc
      (\ev -> T.putStrLn ("Event: " <> T.pack (show ev)))

    runRIO state $ do
      let chain =
            receiveTmNcduC
              .| ncduToTMFrameC
              .| storeFrameC
              .| showConduit
              .| tmFrameExtraction defaultMissionSpecific IF_NCTRS


          showConduitF = awaitForever $ \_du -> pure ()

      runGeneralTCPClient (clientSettings 2502 "localhost") $ \app ->
        void $ concurrently
          ({- runConduitRes (chain .| appSink app)-}
           return ())
          (runConduitRes (appSource app .| chain .| showConduitF))

