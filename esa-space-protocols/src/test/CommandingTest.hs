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
import           Data.Conduit.List
import qualified Data.Conduit.Combinators      as C
import           Data.Conduit.Network

import           Data.PUS.TCTransferFrame
import           Data.PUS.TCTransferFrameEncoder
import           Data.PUS.CLTU
import           Data.PUS.CLTUEncoder
import           Data.PUS.GlobalState
import           Data.PUS.Config
import           General.PUSTypes
import           Data.PUS.PUSPacketEncoder
import           Data.PUS.SegmentEncoder
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



-- pkt1 ssc = PUSPacket
--     (PUSHeader 0 0 PUSTC True (APID 256) SegmentStandalone (mkSSC ssc) 0 0)
--     (PUSTCStdHeader 3 25 (mkSourceID 0) True True False True)
--     Nothing
--     (B.pack [0 .. 10])


-- | Generate a TC Request wehre the parameter n is the number of 'Parameter' values
rqst :: Int -> TCRequest
rqst n = TCRequest 0 (IfNctrs 1) (mkSCID 533) (mkVCID 1) (TCCommand 0 BD (APID 256) (mkPUSType 2) (mkPUSSubType 10) (mkSourceID 10) (List params Empty))
    where params = RIO.replicate n (Parameter "X" (ValUInt3 0b101))


-- | Generate a TC Packet where the parameter n is the number of 'Parameter'
-- tcPacket :: Int -> TCPacket
-- tcPacket n =
--     TCPacket (APID 256) (mkPUSType 2) (mkPUSSubType 10) (mkSourceID 10)
--         $ toSizedParamList (List params Empty)
--     where params = RIO.replicate n (Parameter "X" (ValUInt3 0b101))

packets :: Int -> [EncodedTCRequest]
packets n =
    RIO.map encodeTCRequest $ RIO.replicate n (rqst (256 `div` 3))





main :: IO ()
main = do
    np <- getNumProcessors
    setNumCapabilities np

    defLogOptions <- logOptionsHandle stdout True
    let logOptions = setLogMinLevel LevelError defLogOptions
    withLogFunc logOptions $ \logFunc -> do
        state <- newGlobalState
            defaultConfig
            (defaultMissionSpecific defaultConfig)
            logFunc
            (\ev -> T.putStrLn ("Event: " <> T.pack (show ev)))

        runRIO state $ do
            let chain =
                    sourceList (packets 1000)
                        .| tcPktEncoderC (defaultMissionSpecific defaultConfig)
                        .| tcPktToEncPUSC
                        .| tcSegmentEncoderC
                        .| tcSegmentToTransferFrame
                        .| tcFrameEncodeC
                        .| tcFrameToCltuC
                        .| cltuEncodeRandomizedC
                        .| cltuToNcduC
                        .| encodeTcNcduC

            runGeneralTCPClient (clientSettings 32111 "localhost") $ \app ->
                concurrently_ (runConduitRes (chain .| appSink app))
                    (runConduitRes (appSource app .| receiveTcNcduC .| C.print))

