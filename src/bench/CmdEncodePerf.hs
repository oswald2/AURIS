{-# LANGUAGE 
    OverloadedStrings
    , BangPatterns
    , NoImplicitPrelude
#-}
module Main
where

import RIO 
import           Criterion.Main



pkt1 ssc = PUSPacket
    (PUSHeader 0 0 PUSTC True (APID 256) SegmentStandalone (mkSSC ssc) 0 0)
    (PUSTCStdHeader 3 25 0 True True False True)
    Nothing
    (B.pack [0 .. 10])

rqst1 = TCRequest 0 0 (mkSCID 533) (mkVCID 1)

pusPackets = RIO.map (\i -> (pkt1 i, rqst1)) [1..1000]





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

            defaultMain [
                [ bgroup
                ]
            ]
