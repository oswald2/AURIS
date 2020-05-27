{-# LANGUAGE OverloadedStrings
    , BangPatterns
    , GeneralizedNewtypeDeriving
    , DeriveGeneric
    , RecordWildCards
    , FlexibleInstances
#-}
module Main where


import           RIO
import qualified Data.ByteString               as B
import qualified RIO.Text                      as T
import qualified Data.Text.IO                  as T

import           Conduit
import           Data.Conduit.List
import           Data.Conduit.Network
import           Data.Conduit.TQueue

import           Data.PUS.TCTransferFrame
import           Data.PUS.TCTransferFrameEncoder
import           Data.PUS.CLTU
import           Data.PUS.CLTUEncoder
import           Data.PUS.GlobalState
import           Data.PUS.Config
import           General.PUSTypes
import           Data.PUS.PUSPacket
import           Data.PUS.PUSDfh
import           Data.PUS.PUSPacketEncoder
import           Data.PUS.SegmentEncoder
import           Data.PUS.SegmentationFlags
import           General.APID
import           Data.PUS.TCRequest
import           Data.PUS.MissionSpecific.Definitions
import           Data.PUS.FOP1

import           Protocol.NCTRS
import           Protocol.ProtocolInterfaces
import           Protocol.Switcher

import           GHC.Conc.Sync


pkt1 :: Word16 -> PUSPacket
pkt1 ssc = PUSPacket
    (PUSHeader 0 0 PUSTC True (APID 256) SegmentStandalone (mkSSC ssc) 0 0)
    (PUSTCStdHeader 3 25 (mkSourceID 0) True True False True)
    Nothing
    (B.pack [0 .. 10])

rqst1 = TCRequest 0 (IfNctrs 1) (mkSCID 533) (mkVCID 1) (TCCommand 0 BD)

pusPackets = RIO.map (\i -> (pkt1 i, rqst1)) [1 .. 1000]





main :: IO ()
main = do
    return ()
    -- np <- getNumProcessors
    -- setNumCapabilities np

    -- defLogOptions <- logOptionsHandle stdout True
    -- let logOptions = setLogMinLevel LevelDebug defLogOptions
    -- withLogFunc logOptions $ \logFunc -> do
    --     state <- newGlobalState
    --         defaultConfig
    --         (defaultMissionSpecific defaultConfig)
    --         logFunc
    --         (\ev -> T.putStrLn ("Event: " <> T.pack (show ev)))

    --     runRIO state $ do

    --         channels  <- createProtocolChannels
    --         waitQueue <- liftIO newEmptyTMVarIO
    --         outQueue  <- liftIO $ newTBQueueIO 1000

    --         let chain =
    --                 sourceList pusPackets
    --                     .| pusPacketEncoderC
    --                     .| tcSegmentEncoderC
    --                     .| protocolSwitcherC channels

    --             cop1C = cop1Conduit (channels ^. prChNCTRS) waitQueue outQueue

    --             interfC = sourceTBQueue outQueue
    --                 .| tcFrameEncodeC
    --                 .| tcFrameToCltuC
    --                 .| cltuEncodeRandomizedC
    --                 .| cltuToNcduC
    --                 .| encodeTcNcduC

    --             showConduit = awaitForever $ \ncdu -> liftIO (print ncdu)


    --         runGeneralTCPClient (clientSettings 32111 "localhost") $ \app ->
    --             void
    --                 $   runConc
    --                 $   (,,,)
    --                 <$> conc (runConduitRes chain)
    --                 <*> conc (runConduitRes cop1C)
    --                 <*> conc (runConduitRes (interfC .| appSink app))
    --                 <*> conc
    --                         (runConduitRes
    --                             (appSource app .| receiveTcNcduC .| showConduit)
    --                         )


