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


import           RIO.List
import           RIO.List.Partial               ( head )

import           Control.Lens                   ( (.~) )

import           Data.PUS.TMFrame
import           Data.PUS.TMFrameExtractor
import           Data.PUS.Config
import           Data.PUS.MissionSpecific.Definitions
import           Data.PUS.ExtractedDU
import           Data.PUS.GlobalState
import           Data.PUS.PUSPacket
import           Data.PUS.PUSDfh
import           General.PUSTypes
import           Data.PUS.SegmentationFlags
import           General.APID
import           Data.PUS.EncTime
import           Data.PUS.CLCW
import           Data.PUS.CRC
--import           Data.PUS.TMFrameExtractor

--import           General.Types
import           General.Chunks
import           General.Hexdump

import           Conduit
import qualified Data.Conduit.List             as C

import           Protocol.ProtocolInterfaces

import           Test.Hspec





makeTMFrames
    :: Config -> PUSMissionSpecific -> TMFrameHeader -> ByteString -> [TMFrame]
makeTMFrames cfg missionSpecific hdr pl =
    let len       = tmFrameMaxDataLen cfg missionSpecific hdr
        frameData = chunkedByBS len pl
        hdrs      = map upd [0 ..]
        upd x = hdr & tmFrameVCFC .~ x & tmFrameMCFC .~ x
        frames = zipWith (makeTMFrame (Just (packValues defaultCLCW))) hdrs frameData 
    in  frames


runRIOTestAction :: RIO GlobalState b -> IO b
runRIOTestAction action = do
    defLogOptions <- logOptionsHandle stdout True
    let logOptions = setLogMinLevel LevelError defLogOptions
    withLogFunc logOptions $ \logFunc -> do
        state <- newGlobalState
            defaultConfig
            defaultMissionSpecific
            logFunc
            (\ev -> T.putStrLn ("Event: " <> T.pack (show ev)))

        runRIO state action



pusPacketEncoding :: Config -> IO ()
pusPacketEncoding _cfg = do
    let pusPkt = PUSPacket pusHdr' pusDfh' Nothing payload
        pusHdr' =
            PUSHeader 0 0 PUSTM True (APID 256) SegmentStandalone (mkSSC 10) 0 0
        pusDfh'   = PUSTMStdHeader 0 3 25 (mkSourceID 0) nullCUCTime
        payload   = B.pack (take 4096 (cycle [0 .. 255]))
    --payload   = B.pack [0xaa, 0xbb]
        encPusPkt = encodePUSPacket pusPkt

        decodedPusPkt =
            decodePktMissionSpecific encPusPkt defaultMissionSpecific IF_NCTRS

    -- T.putStrLn $ hexdumpBS encPusPkt
    -- T.putStrLn $ T.pack (show decodedPusPkt)

    isRight decodedPusPkt `shouldBe` True

    let Right (ProtocolPacket _ pkt) = decodedPusPkt

    pkt `shouldBe` pusPkt


pusPacketExtraction :: Config -> IO ()
pusPacketExtraction cfg = do
    let pusPkt = PUSPacket pusHdr' pusDfh' Nothing payload
        pusHdr' =
            PUSHeader 0 0 PUSTM True (APID 256) SegmentStandalone (mkSSC 10) 0 0
        pusDfh'   = PUSTMStdHeader 0 3 25 (mkSourceID 0) nullCUCTime
        payload   = B.pack (take 4096 (cycle [0 .. 255]))
        encPusPkt = encodePUSPacket pusPkt

        frames    = makeTMFrames cfg
                                 defaultMissionSpecific
                                 tmFrameDefaultHeader
                                 encPusPkt

        conduit =
            C.sourceList frames
                .| tmFrameExtraction defaultMissionSpecific IF_NCTRS
                .| C.consume

    --T.putStrLn $ T.pack (show frames)

    result <- runRIOTestAction (runConduit conduit)

    --T.putStrLn $ T.pack (show result)

    length result `shouldBe` 1
    head result ^. epDU `shouldBe` pusPkt
    return ()

testFrameExtraction2 :: IO ()
testFrameExtraction2 = do
    let frame = TMFrame
            { _tmFrameHdr  = TMFrameHeader
                                 { _tmFrameVersion        = 0
                                 , _tmFrameScID = SCID { getSCID = 533 }
                                 , _tmFrameVcID           = VCID { getVCID = 0 }
                                 , _tmFrameOpControl      = True
                                 , _tmFrameMCFC           = 112
                                 , _tmFrameVCFC           = 108
                                 , _tmFrameDfh            = False
                                 , _tmFrameSync           = False
                                 , _tmFrameOrder          = False
                                 , _tmFrameSegID          = TMSegment65536
                                 , _tmFrameFirstHeaderPtr = 0
                                 }
            , _tmFrameData =
                "\b\DC1\192\ETX\NUL\SI\DLE\SOH\SOH\NULJ\158\US\SUB\252\ESC\NUL\NUL\NUL\NUL|\143\a\255\208\213\EOT2UUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUU=\GS"
            , _tmFrameOCF  = Just $ packValues CLCW { _clcwType        = False
                                       , _clcwVersion     = 0
                                       , _clcwStatus      = 0
                                       , _clcwCopInEffect = 1
                                       , _clcwVcID        = VCID { getVCID = 0 }
                                       , _clcwNoRF        = False
                                       , _clcwNoBitLock   = False
                                       , _clcwLockout     = False
                                       , _clcwWait        = False
                                       , _clcwRetrans     = False
                                       , _clcwBCounter    = 0
                                       , _clcwReportType  = False
                                       , _clcwReportVal   = 0
                                       }
            , _tmFrameFECW = Just (mkCRC 61462)
            }
        extractedPacket =
            [ ExtractedDU
                  { _epQuality = toFlag Good True
                  , _epGap     = Nothing
                  , _epSource  = IF_NCTRS
                  , _epDU      =
                      PUSPacket
                          { _pusHdr  =
                              PUSHeader { _pusHdrPktID     = 2065
                                        , _pusHdrTcVersion = 0
                                        , _pusHdrType      = PUSTM
                                        , _pusHdrDfhFlag   = True
                                        , _pusHdrTcApid = APID { getAPID = 17 }
                                        , _pusHdrSeqFlags  = SegmentStandalone
                                        , _pusHdrTcSsc     = mkSSC 3
                                        , _pusHdrSeqCtrl   = 49155
                                        , _pusHdrTcLength  = 15
                                        }
                          , _pusDfh  =
                              PUSTMStdHeader
                                  { _stdTmVersion       = 1
                                  , _stdTmType          = mkPUSType 1
                                  , _stdTmSubType       = mkPUSSubType 1
                                  , _stdTmDestinationID = mkSourceID 0
                                  , _stdTmOBTime        = CUCTime 1251876634
                                                                  (-54525952)
                                                                  False
                                  }
                          , _pusPIs  = Nothing
                          , _pusData = "\NUL\NUL\NUL\NUL"
                          }
                  }
            ]
        conduit =
            C.sourceList [frame]
                .| tmFrameExtraction defaultMissionSpecific IF_NCTRS
                .| C.consume

    result <- runRIOTestAction (runConduit conduit)

    -- T.putStrLn $ T.pack (show result)
    length result `shouldBe` 1
    result `shouldBe` extractedPacket
    return ()


missingFrame :: Config -> IO ()
missingFrame cfg = do
    let pusPkt = PUSPacket pusHdr' pusDfh' Nothing payload
        pusHdr' =
            PUSHeader 0 0 PUSTM True (APID 256) SegmentStandalone (mkSSC 10) 0 0
        pusDfh'   = PUSTMStdHeader 0 3 25 (mkSourceID 0) nullCUCTime
        payload   = B.pack (take 4096 (cycle [0 .. 255]))
        encPusPkt = encodePUSPacket pusPkt

        len       = tmFrameMaxDataLen cfg defaultMissionSpecific tmFrameDefaultHeader
        frameData = chunkedByBS len encPusPkt
        hdrs      = map upd [0 ..]
        upd x = tmFrameDefaultHeader & tmFrameVCFC .~ x & tmFrameMCFC .~ x
        frames'' = zipWith (makeTMFrame (Just (packValues defaultCLCW))) hdrs frameData 

        fhps [] _ = []
        fhps [x] _ = [fromIntegral (B.length x)] 
        fhps (_x:xs) True = 0 : fhps xs False 
        fhps (_x:xs) False = tmFrameNoFirstHeader : fhps xs False 

        fhps' = fhps frameData True
        updFhp x f = x & tmFrameHdr . tmFrameFirstHeaderPtr .~ f  
        frames' = zipWith updFhp frames'' fhps'

        frames = drop2nd frames'

        drop2nd [] = []
        drop2nd [x] = [x]
        drop2nd (x : _y : xs) = x : xs 

        conduit =
            C.sourceList frames
                .| tmFrameExtraction defaultMissionSpecific IF_NCTRS
                .| C.consume

    -- T.putStrLn $ "Frames:\n" <> T.pack (show frames')
    -- T.putStrLn $ "Frames (drop):\n" <> T.pack (show frames)

    result <- runRIOTestAction (runConduit conduit)

    T.putStrLn $ "Result: " <> T.pack (show result)

    length result `shouldBe` 0
    return ()




main :: IO ()
main = hspec $ do
    let cfg = defaultConfig

    describe "TM Frame Extraction" $ do
        it "PUS Packet encoding" $ do
            pusPacketEncoding cfg
        it "PUS Packet extraction" $ do
            pusPacketExtraction cfg
        it "Frame Extraction2" $ do
            testFrameExtraction2
        it "Missing Frame" $ do
            missingFrame cfg
            
            