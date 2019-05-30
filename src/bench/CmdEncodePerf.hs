{-# LANGUAGE 
    OverloadedStrings
    , BangPatterns
    , NoImplicitPrelude
#-}
module Main where

import           RIO
import qualified RIO.ByteString                as B
import qualified RIO.Text                      as T
import qualified Data.Text.IO                  as T
import           Criterion.Main
import           Conduit
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
import           Data.PUS.MissionSpecific.Definitions

import           Protocol.NCTRS

import           GHC.Conc.Sync



pkt1 len ssc = PUSPacket
    (PUSHeader 0 0 PUSTC True (APID 256) SegmentStandalone (mkSSC ssc) 0 0)
    (PUSTCStdHeader 3 25 0 True True False True)
    Nothing
    (B.replicate len 0xaa)

rqst1 = TCRequest 0 0 (mkSCID 533) (mkVCID 1)

pusPackets len = RIO.map (\i -> (pkt1 len i, rqst1)) [1 .. 1000]





main :: IO ()
main = do
    np <- getNumProcessors
    setNumCapabilities np

    defLogOptions <- logOptionsHandle stdout True
    let logOptions = setLogMinLevel LevelError defLogOptions
    withLogFunc logOptions $ \logFunc -> do
        state <- newGlobalState
            Data.PUS.Config.defaultConfig
            defaultMissionSpecific
            logFunc
            (\ev -> T.putStrLn ("Event: " <> T.pack (show ev)))

        
        let chain len =
                sourceList (pusPackets len)
                    .| pusPacketEncoderC
                    .| tcSegmentEncoderC
                    .| tcSegmentToTransferFrame
                    .| tcFrameEncodeC
                    .| tcFrameToCltuC
                    .| cltuEncodeRandomizedC
                    .| cltuToNcduC
                    .| encodeTcNcduC
                    .| finalSink

            --showConduit = awaitForever $ \ncdu -> liftIO (print ncdu)
            finalSink :: (Monad m) => ConduitT ByteString Void m ()
            finalSink = do
                x <- await 
                case x of 
                    Just _ -> finalSink
                    Nothing -> pure ()
                

        defaultMain
            [ bgroup "encoding"
                        [
                        bench "Encode10" $ whnfIO $ runRIO state (runConduit (chain 10))
                        , bench "Encode100" $ whnfIO $ runRIO state (runConduit (chain 100))
                        , bench "Encode248" $ whnfIO $ runRIO state (runConduit (chain 248))
                        , bench "Encode1024" $ whnfIO $ runRIO state (runConduit (chain 1024))
                        ]
            ]
