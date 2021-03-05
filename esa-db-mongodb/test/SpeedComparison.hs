module Main
    ( main
    ) where

import           RIO
import           RIO.Partial                    ( read
                                                , fromJust
                                                )
import qualified RIO.Text                      as T
import qualified Data.Text.IO                  as T

import           Database.MongoDB
import           RIO.List.Partial               ( last )

import           Data.Mongo.Conversion.Class
import           Data.Mongo.Conversion.TMFrame  ( )
import           Data.Mongo.Conversion.ExtractedDU
                                                ( )

import           Data.PUS.TMStoreFrame
import           Data.PUS.TMFrame
import           Data.PUS.CLCW
import           Data.PUS.ExtractedDU

import           General.PUSTypes
import           General.Time
import           General.Types

import           System.Environment
import           Protocol.ProtocolInterfaces
import           Text.Show.Pretty               ( ppShow )



tmFrame :: SunTime -> ExtractedDU TMFrame
tmFrame now =
    let
        storeFrame = ExtractedDU (toFlag Good True)
                                 now
                                 --(Just (1, 2))
                                 Nothing
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

    e2   <- access pipe master "active_session" getFrames

    close pipe



worker :: SunTime -> Int -> Action IO ()
worker now n = do
    delete (select [] "tm_frames")

    start1 <- liftIO getCurrentTime
    replicateM_ n
        $ insertAll_ "tm_frames" (map toDB (replicate 1000 (tmFrame now)))
    end1                        <- liftIO getCurrentTime

    --liftIO $ T.putStrLn $ "IDs: " <> T.pack (show (length ids))

    start2                      <- liftIO getCurrentTime
    (results :: [ExtractedDU TMFrame]) <-
        force
        .   map (fromJust . fromDB)
        <$> (find (select [] "tm_frames") >>= rest)
    let res = force (last results)
    liftIO $ T.putStrLn $ "TM Frame: " <> T.pack (show res)
    end2 <- liftIO getCurrentTime

    liftIO $ T.putStrLn $ "Time to insert: " <> textDisplay (end1 <-> start1)
    liftIO $ T.putStrLn $ "Time to retrieve: " <> textDisplay (end2 <-> start2)

    liftIO $ T.putStrLn $ "Count: " <> T.pack (show (length results))

    return ()


getFrames :: Action IO () 
getFrames = do 
    let start = nullTime 
    stop <- lift getCurrentTime 

    let stopVal = timeToMicro stop


    cursor <- find
        (select ["ert" =: ["$gte" =: Int64 0, "$lte" =: stopVal]]
                "tm_frames"
            )
            -- { sort = ["ert" =: Int32 (-1)]
            -- }
    records <- rest cursor

    liftIO $ T.putStrLn $ "Records retrieved: " <> T.pack (show (length records))
    liftIO $ T.putStrLn $ "Records:\n" <> T.pack (ppShow records)
    let recs :: [ExtractedDU TMFrame] = mapMaybe fromDB $ records 
    liftIO $ T.putStrLn $ "Records mapped: " <> T.pack (show (length recs))
