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
import           Protocol.ProtocolInterfaces

import           GHC.Conc.Sync


-- transferFrames :: [TCTransferFrame]
-- transferFrames =
--     [ TCTransferFrame 0 FrameBD (mkSCID 0) (mkVCID 0) 0 0 (B.replicate 8 0xAA)
--     , TCTransferFrame 0 FrameBD (mkSCID 0) (mkVCID 0) 0 1 (B.replicate 8 0xBB)
--     ]

-- data PUSHeader = PUSHeader {
--     _pusHdrPktID :: !Word16,
--     _pusHdrTcVersion :: !Word8,
--     _pusHdrType ::  !PUSPacketType,
--     _pusHdrDfhFlag :: !Bool,
--     _pusHdrTcApid :: !APID,
--     _pusHdrSeqFlags :: !SegmentationFlags,
--     _pusHdrTcSsc :: !SSC,
--     _pusHdrSeqCtrl :: !Word16,
--     _pusHdrTcLength :: !Word16
--     } deriving(Show, Read, Generic)

-- data PUSPacket = PUSPacket {
--     _pusHdr :: !PUSHeader,
--     _pusDfh :: !DataFieldHeader,
--     _pusPIs :: Maybe (TMPIVal, TMPIVal),
--     _pusData :: !ByteString
--     } deriving (Show, Generic)

-- | PUSStdHeader {
--     _stdType :: PUSType
--     , _stdSubType :: PUSSubType
--     , _stdSrcID :: !Word8
--     , _stdFlagAcceptance :: !Bool
--     , _stdFlagStartExec :: !Bool
--     , _stdFlagProgressExec :: !Bool
--     , _stdFlagExecComp :: !Bool
--     }



pkt1 ssc = PUSPacket
    (PUSHeader 0 0 PUSTC True (APID 256) SegmentStandalone (mkSSC ssc) 0 0)
    (PUSTCStdHeader 3 25 0 True True False True)
    Nothing
    (B.pack [0 .. 10])

rqst1 = TCRequest 0 IF_NCTRS (mkSCID 533) (mkVCID 1) (TCCommand 0 BD)

pusPackets = RIO.map (\i -> (pkt1 i, rqst1)) [1 .. 1000]





main :: IO ()
main = do
    np <- getNumProcessors
    setNumCapabilities np

    defLogOptions <- logOptionsHandle stdout True
    let logOptions = setLogMinLevel LevelDebug defLogOptions
    withLogFunc logOptions $ \logFunc -> do
        state <- newGlobalState
            defaultConfig
            defaultMissionSpecific
            logFunc
            (\ev -> T.putStrLn ("Event: " <> T.pack (show ev)))

        runRIO state $ do
            let chain =
                    sourceList pusPackets
                        .| pusPacketEncoderC
                        .| tcSegmentEncoderC
                        .| tcSegmentToTransferFrame
                        .| tcFrameEncodeC
                        .| tcFrameToCltuC
                        .| cltuEncodeRandomizedC
                        .| cltuToNcduC
                        .| encodeTcNcduC

                showConduit = awaitForever $ \ncdu -> liftIO (print ncdu)

            runGeneralTCPClient (clientSettings 32111 "localhost") $ \app ->
                void $ concurrently
                    (runConduitRes (chain .| appSink app))
                    (runConduitRes
                        (appSource app .| receiveTcNcduC .| showConduit)
                    )
