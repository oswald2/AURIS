{-# LANGUAGE
    OverloadedStrings
    , BangPatterns
    , NoImplicitPrelude
    , BinaryLiterals
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
import           General.PUSTypes
import           Data.PUS.PUSPacket
import           Data.PUS.PUSDfh
import           Data.PUS.PUSPacketEncoder
import           Data.PUS.SegmentEncoder
import           Data.PUS.SegmentationFlags
import           General.APID
import           Data.PUS.TCRequest
import           Data.PUS.TCRequestEncoder
import           Data.PUS.TCPacketEncoder
import           Data.PUS.TCPacket
import           Data.PUS.Parameter
import           Data.PUS.Value
import           Data.PUS.MissionSpecific.Definitions

import           Protocol.NCTRS
import           Protocol.ProtocolInterfaces
import           GHC.Conc.Sync



rqst1 :: TCRequest
rqst1 = TCRequest 0 (IfNctrs 1) (mkSCID 533) (mkVCID 1) (TCCommand 0 BD)

tcPacket :: Int -> TCPacket
tcPacket n = TCPacket (APID 256) (mkPUSType 2) (mkPUSSubType 10) (mkSourceID 10)
  $ toSizedParamList (List params Empty)
  where params = RIO.replicate n (Parameter "X" (ValUInt3 0b101))

packets :: Int -> [EncodedTCRequest]
packets n =
  RIO.replicate 1000 (EncodedTCRequest (Just (tcPacket (n `div` 3))) rqst1)




main :: IO ()
main = do
  np <- getNumProcessors
  setNumCapabilities np

  defLogOptions <- logOptionsHandle stdout True
  let logOptions = setLogMinLevel LevelError defLogOptions
  withLogFunc logOptions $ \logFunc -> do
    state <- newGlobalState
      Data.PUS.Config.defaultConfig
      (defaultMissionSpecific Data.PUS.Config.defaultConfig)
      logFunc
      (\ev -> T.putStrLn ("Event: " <> T.pack (show ev)))


    let chain len =
          sourceList (packets len)
            .| tcPktEncoderC (defaultMissionSpecific Data.PUS.Config.defaultConfig)
            .| tcPktToEncPUSC
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
            Just _  -> finalSink
            Nothing -> pure ()


    defaultMain
      [ bgroup
          "TC Packet encoding"
          [ bench "Encode10" $ whnfIO $ runRIO state (runConduit (chain 10))
          , bench "Encode100" $ whnfIO $ runRIO state (runConduit (chain 100))
          , bench "Encode248" $ whnfIO $ runRIO state (runConduit (chain 248))
          , bench "Encode1024" $ whnfIO $ runRIO state (runConduit (chain 1024))
          ]
      ]
