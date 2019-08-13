{-# LANGUAGE
    OverloadedStrings
    , BangPatterns
    , NoImplicitPrelude
#-}
module Main where


import           RIO


import           Data.PUS.TMFrame
import           Data.PUS.TMFrameExtractor
import           Data.PUS.Config
import           General.Chunks
import           Conduit

import           Test.Hspec




payload :: ByteString
payload = B.pack (take 2400 (cycle [0 .. 255]))


makeTMFrames
    :: Config -> PUSMissionSpecific -> TMFrameHeader -> ByteString -> [TMFrame]
makeTMFrames cfg missionSpecific hdr pl =
    let len       = tmFrameMaxDataLen cfg missionSpecific hdr
        frameData = chunkedByBS len pl
        hdrs      = map upd [0 ..]
        upd x = hdr { _tmFrameVCFC = x, _tmFrameMCFC = x }
        frames = zipWith makeTMFrame hdrs frameData
    in  frames



main :: IO ()
main = hspec $ do
    let cfg = defaultConfig

    describe "TM Frame Extraction" $ do
        it "good extraction" $ do
            let frames = map ep $ makeTMFrames cfg
                                      defaultMissionSpecific
                                      tmFrameDefaultHeader
                                      payload
                ep x = ExtractedDU {
                    _epQuality = toFlag Good True,
                    _epGap = Nothing,
                    _epSource = IF_NCTRS,
                    _epDU = x
                    }
                conduit =
                    sourceList frames .| extractPktFromTMFramesC defaultMissionSpecific IF_NCTRS .| sinkList
            res <- runConduit conduit
            print res
            return ()

