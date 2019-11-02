{-# LANGUAGE
    OverloadedStrings
    , BangPatterns
    , NoImplicitPrelude
#-}
module AurisProcessing
  ( runProcessing
  )
where

import           RIO
--import qualified RIO.Text                      as T
--import qualified Data.Text.IO                  as T

import           Conduit
import           Data.Conduit.Network
import           Conduit.SocketConnector

import           Data.PUS.GlobalState
import           Data.PUS.MissionSpecific.Definitions
import           Data.PUS.TMFrameExtractor
import           Data.PUS.TMPacketProcessing
import           Data.PUS.NcduToTMFrame
import           Protocol.NCTRS
import           Protocol.ProtocolInterfaces

import           Interface.Interface
import           Interface.Events

import           AurisConfig




runProcessing :: AurisConfig -> PUSMissionSpecific -> Interface -> IO ()
runProcessing cfg missionSpecific interface = do
  defLogOptions <- logOptionsHandle stdout True
  let logOptions = setLogMinLevel LevelDebug defLogOptions
  withLogFunc logOptions $ \logFunc -> do
    state <- newGlobalState (aurisPusConfig cfg)
                            missionSpecific
                            logFunc
                            (ifRaiseEvent interface . EventPUS)

    runRIO state $ do
      runTMChain cfg
    pure ()


runTMChain :: AurisConfig -> RIO GlobalState ()
runTMChain cfg = do
  let chain =
        receiveTmNcduC
          .| ncduToTMFrameC
          .| storeFrameC
          .| tmFrameExtraction IF_NCTRS
          .| packetProcessorC
          .| raiseTMPacketC

      ignoreConduit = awaitForever $ \_ -> pure ()

  runGeneralTCPReconnectClient
      (clientSettings (aurisNctrsTMPort cfg) (encodeUtf8 (aurisNctrsHost cfg))) 200000
    $ \app -> void $ runConduitRes (appSource app .| chain .| ignoreConduit)
    -- $ \app -> void $ concurrently
    --     ({- runConduitRes (chain .| appSink app)-}
    --      return ())
    --     (runConduitRes (appSource app .| chain .| ignoreConduit))

