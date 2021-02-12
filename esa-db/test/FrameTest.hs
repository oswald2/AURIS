
import           RIO

import           RIO.Partial                    ( read
                                                )
import qualified RIO.Text                      as T
import qualified Data.Text.IO                  as T
-- import           RIO.List                      ( repeat )
import           RIO.List.Partial               ( last )

import           Data.PUS.TMStoreFrame
import           Data.PUS.TMFrame
import           General.PUSTypes
import           General.Time
import           Data.PUS.CLCW

import           System.Environment


import           Persistence.DbProcessing
import           Persistence.Definitions
import           Persistence.Conversion.Types
import           Persistence.Conversion.TMFrame ( )
import           Data.DbConfig.Postgres
import           Database.Persist


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
    [n] <- getArgs
    now <- getCurrentTime
    worker now (read n)


worker :: SunTime -> Int -> IO ()
worker now n = do
    withPostgres defaultPostgresConfig (deleteWhere ([] :: [Filter DbTMFrame]))

    start1 <- liftIO getCurrentTime
    withPostgres defaultPostgresConfig $ do
        replicateM_ n $ insertMany_ (map toDB (replicate 1000 (tmFrame now)))
    end1     <- liftIO getCurrentTime

    start2   <- liftIO getCurrentTime
    results1 <- withPostgres defaultPostgresConfig $ selectList [] []
    let results :: [TMStoreFrame] = force $ map (fromDB . entityVal) results1
    let res                       = force (last results)
    liftIO $ T.putStrLn $ "TM Frame: " <> T.pack (show res)
    end2 <- liftIO getCurrentTime

    liftIO $ T.putStrLn $ "Time to insert: " <> textDisplay (end1 <-> start1)
    liftIO $ T.putStrLn $ "Time to retrieve: " <> textDisplay (end2 <-> start2)

    liftIO $ T.putStrLn $ "Count: " <> T.pack (show (length results))

    return ()

