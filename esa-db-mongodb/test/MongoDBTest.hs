import           RIO
import qualified RIO.ByteString                as B
import qualified RIO.Vector                    as V
import           RIO.Partial                    ( read
                                                , fromJust
                                                )
import qualified RIO.Text                      as T
import qualified Data.Text.IO                  as T
import           RIO.List                       ( cycle )
import           Database.MongoDB
import           RIO.List.Partial               ( last )

import           Data.Mongo.Conversion.Class
import           Data.Mongo.Conversion.TMFrame  ( )


import           Data.PUS.TMFrame
import           Data.PUS.ExtractedDU
import           Data.PUS.TMFrame
import           Data.PUS.PUSPacket
import           Data.PUS.PUSDfh
import           Data.PUS.SegmentationFlags
import           General.PUSTypes
import           General.Types
import           General.Time
import           General.APID
import           Data.PUS.EncTime
import           Data.PUS.CLCW
import           Data.PUS.TMPacket
import           Persistence.LogEvent
import           Data.TM.Parameter
import           Data.TM.Value
import           Data.TM.Validity
import           Data.TM.TMPacketDef            ( PIDEvent(..) )
import           Protocol.ProtocolInterfaces

import qualified Data.Time.Clock               as T
import           System.Environment
import           Data.Mongo.Conversion.LogEvent
import           Data.Mongo.Conversion.TMFrame
import           Data.Mongo.Conversion.PUSPacket
import           Data.Mongo.Conversion.TMPacket
import           Data.Mongo.Conversion.ExtractedDU

import           Test.Hspec


tmFrame :: SunTime -> ExtractedDU TMFrame
tmFrame now =
    let
        storeFrame = ExtractedDU (toFlag Good True)
                                 now
                                 (Just (1, 2))
                                 --Nothing
                                 (IfNctrs 1)
                                 (IsVCID 0)
                                 frame
            --(HexBytes (encodeFrame defaultTMFrameConfig frame))
        frame = TMFrame
            { _tmFrameHdr  = TMFrameHeader { _tmFrameVersion        = 0
                                           , _tmFrameScID           = SCID 533
                                           , _tmFrameVcID           = VCID 0
                                           , _tmFrameOpControl      = True
                                           , _tmFrameMCFC           = 112
                                           , _tmFrameVCFC           = 108
                                           , _tmFrameDfh            = False
                                           , _tmFrameSync           = False
                                           , _tmFrameOrder          = False
                                           , _tmFrameSegID = TMSegment65536
                                           , _tmFrameFirstHeaderPtr = 0
                                           }
            , _tmFrameSecHdr = TMFrameEmptySecHeader
            , _tmFrameData =
                "\b\DC1\192\ETX\NUL\SI\DLE\SOH\SOH\NULJ\158\US\SUB\252\ESC\NUL\NUL\NUL\NUL|\143\a\255\208\213\EOT2UUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUU=\GS"
            , _tmFrameOCF  = Just $ packValues CLCW { _clcwType        = False
                                                    , _clcwVersion     = 0
                                                    , _clcwStatus      = 0
                                                    , _clcwCopInEffect = 1
                                                    , _clcwVcID        = VCID 0
                                                    , _clcwNoRF        = False
                                                    , _clcwNoBitLock   = False
                                                    , _clcwLockout     = False
                                                    , _clcwWait        = False
                                                    , _clcwRetrans     = False
                                                    , _clcwBCounter    = 0
                                                    , _clcwReportType  = False
                                                    , _clcwReportVal   = 0
                                                    }
            , _tmFrameFECW = Nothing
            }
    in
        storeFrame



main :: IO ()
main = hspec $ do
    describe "Conversion Tests" $ do
        it "TMFRame Conversion" $ do
            now <- getCurrentTime
            let frame = tmFrame now
                doc   = toDB frame
                back  = fromDB doc

            isJust back `shouldBe` True
            frame `shouldBe` fromJust back

        it "Log Conversion" $ do
            now <- T.getCurrentTime
            let logEvent = LogEvent
                    now
                    "Source"
                    LevelDebug
                    (display @Text "This is a test message")
                doc  = toDB logEvent
                back = fromDB doc

            isJust back `shouldBe` True
            logEvent `shouldBe` fromJust back

        it "PUS Packet Header Conversion" $ do
            let
                pusHdr = PUSHeader 0
                                   0
                                   PUSTM
                                   True
                                   (APID 256)
                                   SegmentStandalone
                                   (mkSSC 10)
                                   0
                                   0

            let doc  = toDB pusHdr
                back = fromDB doc

            isJust back `shouldBe` True
            pusHdr `shouldBe` fromJust back

        it "PUS DFH Conversion" $ do
            let pusDfh = PUSTMStdHeader 0 3 25 (mkSourceID 0) (nullCUCTime Cuc42)

            let doc  = toDB pusDfh
                back = fromDB doc

            isJust back `shouldBe` True
            pusDfh `shouldBe` fromJust back

        it "PUS TMPIVal Conversion" $ do
            let val1 :: Maybe (TMPIVal, TMPIVal)
                val1  = Nothing
                val2  = Just (tmpi1, tmpi2)

                tmpi1 = TMPIVal 1001 (ByteOffset 23) (BitSize 32)
                tmpi2 = TMPIVal 123456789123456 (ByteOffset 768) (BitSize 17)

            let docTmpi1 = val tmpi1
                docTmpi2 = val tmpi2
                back1    = cast' docTmpi1
                back2    = cast' docTmpi2

                doc1     = toDB val1
                backDoc1 = fromDB doc1
                doc2     = toDB val2
                backDoc2 = fromDB doc2

            -- T.putStrLn $ "docTmpi1: " <> T.pack (show docTmpi1)
            -- T.putStrLn $ "docTmpi2: " <> T.pack (show docTmpi2)

            -- T.putStrLn $ "doc1: " <> T.pack (show doc1)
            -- T.putStrLn $ "backDoc1: " <> T.pack (show backDoc1)
            -- T.putStrLn $ "doc2: " <> T.pack (show doc2)
            -- T.putStrLn $ "backDoc2: " <> T.pack (show backDoc2)

            isJust back1 `shouldBe` True
            tmpi1 `shouldBe` fromJust back1
            isJust back2 `shouldBe` True
            tmpi2 `shouldBe` fromJust back2

            isJust backDoc1 `shouldBe` True
            val1 `shouldBe` fromJust backDoc1

            isJust backDoc2 `shouldBe` True
            val2 `shouldBe` fromJust backDoc2

        it "HexBytes Conversion" $ do
            let payload = HexBytes $ B.pack (take 4096 (cycle [0 .. 255]))

                doc     = val payload
                back    = cast' doc

            isJust back `shouldBe` True
            payload `shouldBe` fromJust back


        it "PUS Packet Conversion" $ do
            now <- getCurrentTime
            let pusPkt =
                    PUSPacket pusHdr' pusDfh' Nothing (HexBytes payload) True
                pusHdr' = PUSHeader 0
                                    0
                                    PUSTM
                                    True
                                    (APID 256)
                                    SegmentStandalone
                                    (mkSSC 10)
                                    0
                                    0
                pusDfh' = PUSTMStdHeader 0 3 25 (mkSourceID 0) (nullCUCTime Cuc42)
                payload = B.pack (take 10 (cycle [0 .. 255]))

            let doc  = toDB pusPkt
                back = fromDB doc

            -- T.putStrLn $ "doc: " <> T.pack (show doc)
            -- T.putStrLn $ "back: " <> T.pack (show back)

            isJust back `shouldBe` True
            fromJust back `shouldBe` pusPkt

        it "TM Packet Conversion" $ do
            now <- getCurrentTime
            let pkt = TMPacket { _tmpSPID      = SPID 10
                               , _tmpMnemonic  = "Mnemonic"
                               , _tmpDescr     = "Description"
                               , _tmpAPID      = APID 11
                               , _tmpType      = PUSType 3
                               , _tmpSubType   = PUSSubType 25
                               , _tmpPI1       = 0
                               , _tmpPI2       = 0
                               , _tmpERT       = now
                               , _tmpTimeStamp = now
                               , _tmpVCID      = IsVCID (VCID 1)
                               , _tmpSSC       = mkSSC 12345
                               , _tmpEvent     = PIDNo
                               , _tmpSource    = IfNctrs 1
                               , _tmpParams    = params
                               }
                params = V.fromList
                    [ TMParameter
                        { _pName     = "Param1"
                        , _pTime     = now
                        , _pValue    = TMValue (TMValUInt 0xffffffffffffffff)
                                               clearValidity
                        , _pEngValue = Nothing
                        }
                    , TMParameter
                        { _pName     = "Param2"
                        , _pTime     = now
                        , _pValue = TMValue (TMValDouble 3.1415) clearValidity
                        , _pEngValue = Just
                            (TMValue (TMValOctet payl) clearValidity)
                        }
                    ]
                payl = B.pack [1 .. 255]

            let doc  = val pkt
                back = cast' doc

            -- T.putStrLn $ "doc: " <> T.pack (show doc)
            -- T.putStrLn $ "back: " <> T.pack (show back)

            isJust back `shouldBe` True
            fromJust back `shouldBe` pkt
