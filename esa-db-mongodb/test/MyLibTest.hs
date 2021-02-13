module Main
    ( main
    ) where

import           RIO
import           RIO.Partial                    ( read, fromJust )
import qualified RIO.Text                      as T
import qualified Data.Text.IO                  as T
-- import           RIO.List                      ( repeat )
import           Database.MongoDB
import           RIO.List.Partial               ( last )

import           Data.Mongo.Conversion.Class
import           Data.Mongo.Conversion.TMFrame ()


import           Data.PUS.TMStoreFrame
import Data.PUS.TMFrame
    ( TMFrameHeader(TMFrameHeader, _tmFrameVersion, _tmFrameScID,
                    _tmFrameVcID, _tmFrameOpControl, _tmFrameMCFC, _tmFrameVCFC,
                    _tmFrameDfh, _tmFrameSync, _tmFrameOrder, _tmFrameSegID,
                    _tmFrameFirstHeaderPtr),
      TMFrame(TMFrame, _tmFrameHdr, _tmFrameData, _tmFrameOCF,
              _tmFrameFECW),
      defaultTMFrameConfig,
      encodeFrame )
import           General.PUSTypes
import           General.Time
import           Data.PUS.CLCW

import           System.Environment



tmFrame :: SunTime -> TMStoreFrame
tmFrame now =
    let
        storeFrame =
            TMStoreFrame now frame (encodeFrame defaultTMFrameConfig frame)
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
main = do
    [n]  <- getArgs

    pipe <- connect (host "127.0.0.1")
    now  <- getCurrentTime
    e    <- access pipe master "active_session" (worker now (read n))
    close pipe
    T.putStrLn (T.pack (show e))

worker :: SunTime -> Int -> Action IO ()
worker now n = do
    delete (select [] "tm_frames")

    start1 <- liftIO getCurrentTime
    replicateM_ n $ insertAll_ "tm_frames" (map toDB (replicate 1000 (tmFrame now)))
    end1                              <- liftIO getCurrentTime

    --liftIO $ T.putStrLn $ "IDs: " <> T.pack (show (length ids))

    start2                            <- liftIO getCurrentTime
    (results :: [TMStoreFrame]) <-
        force
        . map (fromJust . fromDB)
        <$> (   find (select [] "tm_frames") 
            >>= rest
            )
    let res = force (last results)
    liftIO $ T.putStrLn $ "TM Frame: " <> T.pack (show res)
    end2 <- liftIO getCurrentTime

    liftIO $ T.putStrLn $ "Time to insert: " <> textDisplay (end1 <-> start1)
    liftIO $ T.putStrLn $ "Time to retrieve: " <> textDisplay (end2 <-> start2)

    liftIO $ T.putStrLn $ "Count: " <> T.pack (show (length results))

    return ()
