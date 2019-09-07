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
import           Data.PUS.Types
import           Data.PUS.SegmentationFlags
import           Data.PUS.APID
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
      frames = zipWith makeTMFrame hdrs frameData
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

goodExtraction :: Config -> IO ()
goodExtraction cfg = do
  let payload = B.pack (take 4096 (cycle [0 .. 255]))
  let frames = map ep
        $ makeTMFrames cfg defaultMissionSpecific tmFrameDefaultHeader payload
      ep x = ExtractedDU { _epQuality = toFlag Good True
                         , _epGap     = Nothing
                         , _epSource  = IF_NCTRS
                         , _epDU      = x
                         }
      conduit =
        C.sourceList frames
          .| extractPktFromTMFramesC defaultMissionSpecific IF_NCTRS
          .| C.consume
  result <- runRIOTestAction (runConduit conduit)

  length result `shouldBe` 3
  B.concat result `shouldBe` payload
  return ()


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
  let
    pusPkt = PUSPacket pusHdr' pusDfh' Nothing payload
    pusHdr' =
      PUSHeader 0 0 PUSTM True (APID 256) SegmentStandalone (mkSSC 10) 0 0
    pusDfh'   = PUSTMStdHeader 0 3 25 (mkSourceID 0) nullCUCTime
    payload   = B.pack (take 4096 (cycle [0 .. 255]))
    encPusPkt = encodePUSPacket pusPkt

    frames =
      makeTMFrames cfg defaultMissionSpecific tmFrameDefaultHeader encPusPkt

    conduit =
      C.sourceList frames
        .| tmFrameExtraction defaultMissionSpecific IF_NCTRS
        .| C.consume

  T.putStrLn $ T.pack (show frames)

  result <- runRIOTestAction (runConduit conduit)

  T.putStrLn $ T.pack (show result)

  length result `shouldBe` 1
  head result ^. epDU `shouldBe` pusPkt
  return ()

testFrameExtraction2 :: IO ()
testFrameExtraction2 = do
  let
    frame = TMFrame
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
      , _tmFrameOCF  = Just
                         (CLCW { _clcwType        = False
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
                         )
      , _tmFrameFECW = Just (mkCRC 61462)
      }
    conduit =
      C.sourceList [frame]
        .| tmFrameExtraction defaultMissionSpecific IF_NCTRS
        .| C.consume

  result <- runRIOTestAction (runConduit conduit)

  T.putStrLn $ T.pack (show result)
  length result `shouldBe` 2

  return ()



main :: IO ()
main = hspec $ do
  let cfg = defaultConfig

  describe "TM Frame Extraction" $ do
    -- it "good extraction" $ do
    --   goodExtraction cfg
    -- it "PUS Packet encoding" $ do
    --   pusPacketEncoding cfg
    -- it "PUS Packet extraction" $ do
    --   pusPacketExtraction cfg
    it "Frame Extraction2" $ do
      testFrameExtraction2

