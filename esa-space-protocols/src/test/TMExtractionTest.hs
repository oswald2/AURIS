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
import           Data.PUS.TMStoreFrame
import           Data.PUS.Config
import           Data.PUS.MissionSpecific.Definitions
import           Data.PUS.ExtractedDU
import           Data.PUS.ExtractedPUSPacket
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

import           General.Types
import           General.Chunks
import           General.Hexdump
import           General.Time
import           General.GetBitField

import           Conduit
import qualified Data.Conduit.List             as C

import           Protocol.ProtocolInterfaces

import           Test.Hspec





makeTMFrames
  :: Config -> PUSMissionSpecific -> TMFrameHeader -> ByteString -> [TMFrame]
makeTMFrames cfg missionSpecific hdr pl =
  let
    len       = tmFrameMaxDataLen cfg missionSpecific hdr
    frameData = chunkedByBS len pl
    hdrs      = map upd [0 ..]
    upd x = hdr & tmFrameVCFC .~ x & tmFrameMCFC .~ x
    frames =
      zipWith (makeTMFrame (Just (packValues defaultCLCW))) hdrs frameData
  in
    frames

makeStoreFrames
  :: Config
  -> PUSMissionSpecific
  -> SunTime
  -> TMFrameHeader
  -> ByteString
  -> [TMStoreFrame]
makeStoreFrames cfg missionSpecific timestamp hdr pl =
  let frames      = makeTMFrames cfg missionSpecific hdr pl
      bytes       = map (encodeFrame cfg) frames
      times       = iterate (<+> oneMicroSecond) timestamp
      storeFrames = zipWith3 TMStoreFrame times frames bytes
  in  storeFrames


frameToStoreFrame :: Config -> SunTime -> TMFrame -> TMStoreFrame
frameToStoreFrame cfg timestamp frame =
  TMStoreFrame timestamp frame (encodeFrame cfg frame)


runRIOTestAction :: RIO GlobalState b -> IO b
runRIOTestAction action = do
  defLogOptions <- logOptionsHandle stdout True
  let logOptions = setLogMinLevel LevelError defLogOptions
  withLogFunc logOptions $ \logFunc -> do
    state <- newGlobalState
      defaultConfig
      (defaultMissionSpecific defaultConfig)
      logFunc
      (\ev -> T.putStrLn ("Event: " <> T.pack (show ev)))

    runRIO state action



pusPacketEncoding :: Config -> IO ()
pusPacketEncoding _cfg = do
  let pusPkt = PUSPacket pusHdr' pusDfh' Nothing payload
      pusHdr' =
        PUSHeader 0 0 PUSTM True (APID 256) SegmentStandalone (mkSSC 10) 0 0
      pusDfh'       = PUSTMStdHeader 0 3 25 (mkSourceID 0) nullCUCTime
      payload       = B.pack (take 4096 (cycle [0 .. 255]))
  --payload   = B.pack [0xaa, 0xbb]
      encPusPkt     = encodePUSPacket pusPkt

      decodedPusPkt = decodePktMissionSpecific
        encPusPkt
        (defaultMissionSpecific defaultConfig)
        IF_NCTRS

  -- T.putStrLn $ hexdumpBS encPusPkt
  -- T.putStrLn $ T.pack (show decodedPusPkt)

  isRight decodedPusPkt `shouldBe` True

  let Right (ProtocolPacket _ pkt) = decodedPusPkt

  pkt `shouldBe` pusPkt


pusPacketExtraction :: Config -> IO ()
pusPacketExtraction cfg = do
  now <- getCurrentTime
  let pusPkt = PUSPacket pusHdr' pusDfh' Nothing payload
      pusHdr' =
        PUSHeader 0 0 PUSTM True (APID 256) SegmentStandalone (mkSSC 10) 0 0
      pusDfh'   = PUSTMStdHeader 0 3 25 (mkSourceID 0) nullCUCTime
      payload   = B.pack (take 4096 (cycle [0 .. 255]))
      encPusPkt = encodePUSPacket pusPkt

      frames    = makeStoreFrames cfg
                                  (defaultMissionSpecific cfg)
                                  now
                                  tmFrameDefaultHeader
                                  encPusPkt

      conduit = C.sourceList frames .| tmFrameExtraction IF_NCTRS .| C.consume

  --T.putStrLn $ T.pack (show frames)

  result <- runRIOTestAction (runConduit conduit)

  --T.putStrLn $ T.pack (show result)

  length result `shouldBe` 1
  (_extrPacket . head) result ^. epDU `shouldBe` pusPkt
  return ()

testFrameExtraction2 :: Config -> IO ()
testFrameExtraction2 cfg = do
  now <- getCurrentTime
  let
    storeFrame = TMStoreFrame now frame (encodeFrame cfg frame)
    frame      = TMFrame
      { _tmFrameHdr  = TMFrameHeader { _tmFrameVersion        = 0
                                     , _tmFrameScID = SCID { getSCID = 533 }
                                     , _tmFrameVcID = VCID { getVCID = 0 }
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
                                              , _clcwVcID = VCID { getVCID = 0 }
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
    extractedPacket = ExtractedDU
      { _epQuality = toFlag Good True
      , _epGap     = Nothing
      , _epSource  = IF_NCTRS
      , _epERT     = now
      , _epVCID    = mkVCID 0
      , _epDU      = PUSPacket
                       { _pusHdr = PUSHeader { _pusHdrPktID     = 2065
                                             , _pusHdrTcVersion = 0
                                             , _pusHdrType      = PUSTM
                                             , _pusHdrDfhFlag   = True
                                             , _pusHdrAPID = APID { getAPID = 17 }
                                             , _pusHdrSeqFlags = SegmentStandalone
                                             , _pusHdrSSC       = mkSSC 3
                                             , _pusHdrSeqCtrl   = 49155
                                             , _pusHdrTcLength  = 15
                                             }
                       , _pusDfh  = PUSTMStdHeader
                                      { _stdTmVersion       = 1
                                      , _stdTmType          = mkPUSType 1
                                      , _stdTmSubType       = mkPUSSubType 1
                                      , _stdTmDestinationID = mkSourceID 0
                                      , _stdTmOBTime = CUCTime 1251876634 984787 False
                                      }
                       , _pusPIs  = Nothing
                       , _pusData = "\NUL\NUL\NUL\NUL"
                       }
      }

    conduit =
      C.sourceList [storeFrame] .| tmFrameExtraction IF_NCTRS .| C.consume

  result <- runRIOTestAction (runConduit conduit)

  -- T.putStrLn $ T.pack (show result)
  length result `shouldBe` 1
  (_extrPacket . head) result `shouldBe` extractedPacket
  return ()


missingFrame :: Config -> IO ()
missingFrame cfg = do
  now <- getCurrentTime
  let
    pusPkt = PUSPacket pusHdr' pusDfh' Nothing payload
    pusHdr' =
      PUSHeader 0 0 PUSTM True (APID 256) SegmentStandalone (mkSSC 10) 0 0
    pusDfh'   = PUSTMStdHeader 0 3 25 (mkSourceID 0) nullCUCTime
    payload   = B.pack (take 4096 (cycle [0 .. 255]))
    encPusPkt = encodePUSPacket pusPkt

    len =
      tmFrameMaxDataLen cfg (defaultMissionSpecific cfg) tmFrameDefaultHeader
    frameData = chunkedByBS len encPusPkt
    hdrs      = map upd [0 ..]
    upd x = tmFrameDefaultHeader & tmFrameVCFC .~ x & tmFrameMCFC .~ x
    frames'' =
      zipWith (makeTMFrame (Just (packValues defaultCLCW))) hdrs frameData

    fhps []        _     = []
    fhps [x      ] _     = [fromIntegral (B.length x)]
    fhps (_x : xs) True  = 0 : fhps xs False
    fhps (_x : xs) False = tmFrameNoFirstHeader : fhps xs False

    fhps' = fhps frameData True
    updFhp x f = x & tmFrameHdr . tmFrameFirstHeaderPtr .~ f
    frames'     = zipWith updFhp frames'' fhps'

    frames      = drop2nd frames'

    times       = iterate (<+> oneMicroSecond) now
    storeFrames = zipWith (frameToStoreFrame cfg) times frames

    drop2nd []            = []
    drop2nd [x          ] = [x]
    drop2nd (x : _y : xs) = x : xs

    conduit =
      C.sourceList storeFrames .| tmFrameExtraction IF_NCTRS .| C.consume

  -- T.putStrLn $ "Frames:\n" <> T.pack (show frames')
  -- T.putStrLn $ "Frames (drop):\n" <> T.pack (show frames)

  result <- runRIOTestAction (runConduit conduit)

  T.putStrLn $ "Result: " <> T.pack (show result)

  length result `shouldBe` 0
  return ()



bitGetTest1 :: IO ()
bitGetTest1 = do
  let testData = B.pack [0x01, 0x40]
      result =
        getBitField testData (mkOffset (ByteOffset 0) (BitOffset 7)) (BitSize 3)

  result `shouldBe` Just 5
  return ()

bitGetTest2 :: IO ()
bitGetTest2 = do
  let testData = B.pack [0x01, 0x40, 0x80] -- 10 1000 0001
      result   = getBitField testData
                             (mkOffset (ByteOffset 0) (BitOffset 7))
                             (BitSize 10)

  result `shouldBe` Just 0x281
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
      testFrameExtraction2 cfg
    it "Missing Frame" $ do
      missingFrame cfg

  describe "BitGet Tests" $ do
    it "BitGet test1" $ do
      bitGetTest1
    it "BitGet test2" $ do
      bitGetTest2

