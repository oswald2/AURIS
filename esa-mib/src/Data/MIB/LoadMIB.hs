module Data.MIB.LoadMIB
    ( loadMIB
    , loadCalibs
    , loadSyntheticParameters
    , loadParameters
    , loadPackets
    )
where


import           RIO
import qualified RIO.Vector                    as V
import qualified RIO.HashMap                   as HM
import qualified RIO.Text                      as T
import           RIO.Directory
import           RIO.FilePath
import           RIO.List                       ( intersperse )

import           Control.Monad.Except

import           Data.HashTable.ST.Basic        ( IHashTable )
import qualified Data.HashTable.ST.Basic       as HT
import qualified Data.HashTable.Class          as HTC

import           Data.Either
import           Data.Text.Short                ( ShortText )
import qualified Data.MIB.CAF                  as CAF
import qualified Data.MIB.CAP                  as CAP
import qualified Data.MIB.MCF                  as MCF
import qualified Data.MIB.LGF                  as LGF
import qualified Data.MIB.TXP                  as TXP
import qualified Data.MIB.TXF                  as TXF
import qualified Data.MIB.PCF                  as PCF
import qualified Data.MIB.CUR                  as CUR
import qualified Data.MIB.PID                  as PID
import qualified Data.MIB.TPCF                 as TPCF
import qualified Data.MIB.PLF                  as PLF
import qualified Data.MIB.PIC                  as PIC
import qualified Data.MIB.VPD                  as VPD

import           Data.DataModel

import           Data.TM.Calibration
import           Data.TM.NumericalCalibration
import           Data.TM.PolynomialCalibration
import           Data.TM.LogarithmicCalibration
import           Data.TM.TextualCalibration
import           Data.TM.Synthetic
import           Data.TM.TMParameterDef
import           Data.TM.TMPacketDef

import           Data.Conversion.Calibration
import           Data.Conversion.Parameter
import           Data.Conversion.Types
import           Data.Conversion.TMPacket
import           Data.Conversion.GRD


-- | load the whole MIB into a data structure
loadMIB
    :: (MonadUnliftIO m, MonadReader env m, HasLogFunc env)
    => FilePath
    -> m (Either Text DataModel)
loadMIB mibPath = do
    handleIO
            (\e -> return $
                    Left $"Error on loading MIB: " <> T.pack (displayException e)
            )
        $ runExceptT $ do
          syns      <- loadSyntheticParameters mibPath      >>= liftEither
          calibs    <- loadCalibs mibPath                   >>= liftEither
          params    <- loadParameters mibPath calibs syns   >>= liftEither >>= logWarnings
          vpds      <- VPD.loadFromFile mibPath             >>= liftEither
          vpdLookup <- pure (generateVPDLookup vpds params) >>= liftEither
          pics      <- PIC.loadFromFile mibPath             >>= liftEither
          let !pIdx = picSeachIndexFromPIC pics
          packets   <- loadPackets mibPath params vpdLookup >>= liftEither >>= logWarnings
          grds      <- loadGRDs mibPath                     >>= liftEither
          return $ DataModel
              { _dmCalibrations    = calibs
              , _dmSyntheticParams = syns
              , _dmParameters      = params
              , _dmPacketIdIdx     = pIdx
              , _dmTMPackets       = packets
              , _dmVPDStructs      = vpdLookup
              , _dmGRDs = grds
              }

  where
    logWarnings = \case
        (Nothing, res) -> return res
        (Just w, res) -> do
            logWarn $ "On parameter import: " <> display w
            return res


loadParameters
    :: (MonadIO m, MonadReader env m, HasLogFunc env)
    => FilePath
    -> HashMap ShortText Calibration
    -> HashMap ShortText Synthetic
    -> m
           ( Either
                 Text
                 (Maybe Text, IHashTable ShortText TMParameterDef)
           )
loadParameters mibPath calibHM synthHM = runExceptT $ do
    pcfs <- PCF.loadFromFile mibPath >>= liftEither
    curs <- CUR.loadFromFile mibPath >>= liftEither
    pure (convertParameters pcfs curs calibHM synthHM) >>= liftEither



-- | load all calibrations and return either an error or a
-- 'HashMap' containing all the 'Calibration's.
loadCalibs
    :: (MonadIO m, MonadReader env m, HasLogFunc env)
    => FilePath
    -> m (Either Text (HashMap ShortText Calibration))
loadCalibs mibPath = do
    runExceptT $ do
        -- load calibrations
        !cafs <- CAF.loadFromFile mibPath
        !caps <- CAP.loadFromFile mibPath
        !mcfs <- MCF.loadFromFile mibPath
        !lgfs <- LGF.loadFromFile mibPath
        !txps <- TXP.loadFromFile mibPath
        !txfs <- TXF.loadFromFile mibPath

        let caf = fromRight V.empty cafs
            cap = fromRight V.empty caps
            mcf = fromRight V.empty mcfs
            lgf = fromRight V.empty lgfs
            txp = fromRight V.empty txps
            txf = fromRight V.empty txfs


        numCalibs' <- liftEither $ traverse (`convertNumCalib` cap) caf
        let !numCalibs =
                HM.fromList
                    . map (\x -> (_calibNName x, CalibNum x))
                    . toList
                    $ numCalibs'

        polyCalibs' <- liftEither $ traverse convertPolyCalib mcf
        let !polyCalibs = V.foldl'
                (\hm x -> HM.insert (_calibPName x) (CalibPoly x) hm)
                numCalibs
                polyCalibs'

        logCalibs' <- liftEither $ traverse convertLogCalib lgf
        let !logCalibs = V.foldl'
                (\hm x -> HM.insert (_calibLName x) (CalibLog x) hm)
                polyCalibs
                logCalibs'

        textCalibs' <- liftEither $ traverse (`convertTextCalib` txp) txf
        let !textCalibs = V.foldl'
                (\hm x -> HM.insert (_calibTName x) (CalibText x) hm)
                logCalibs
                textCalibs'

        return textCalibs


loadSyntheticParameters
    :: (MonadIO m) => FilePath -> m (Either Text (HashMap ShortText Synthetic))
loadSyntheticParameters path' = do
    let path = path' </> "synthetic"
    doesDirectoryExist path >>= \x -> if not x
        then
            do
                pure
            $  Left
            $  "Could not read synthetic parameters: directory '"
            <> T.pack path
            <> "' does not exist"
        else do
            -- traceM ("Path: " <> (T.pack path))
            files' <- listDirectory path
            files  <- filterM doesFileExist (map (path </>) files')
            -- traceM ("files: " <> (T.pack (show files)))
            ols    <- forM files parseOL
            -- traceM ("ols: " <> (T.pack (show ols)))
            if all isRight ols
                then do
                    let syn = zipWith f files (rights ols)
                        f p ol = (fromString (takeFileName p), ol)
                        !hm = HM.fromList syn
                    loadHCsynths hm
                else
                    do
                        return
                    $  Left
                    $  T.concat
                    $  ["Error parsing synthetic parameters: " :: Text]
                    <> intersperse "\n" (lefts ols)
    where
      loadHCsynths synths = do
        let path = path' </> "hcsynthetic"
        doesDirectoryExist path >>= \x -> if not x
            then
                do
                    pure
                $  Left
                $  "Could not read  hard-coded synthetic parameters: directory '"
                <> T.pack path
                <> "' does not exist"
            else do
                -- traceM ("Path: " <> (T.pack path))
                files' <- listDirectory path
                files  <- filterM doesFileExist (map (path </>) files')
                -- traceM ("files: " <> (T.pack (show files)))
                ols    <- forM files parseOL
                -- traceM ("ols: " <> (T.pack (show ols)))
                if all isRight ols
                    then do
                        let syn = zipWith f files (rights ols)
                            f p ol = (fromString (takeFileName p), ol)
                            !hm = foldl' (\h (n, s) -> HM.insert n s h) synths syn
                        return (Right hm)
                    else
                        do
                            return
                        $  Left
                        $  T.concat
                        $  ["Error parsing synthetic parameters: " :: Text]
                        <> intersperse "\n" (lefts ols)


loadPackets :: (MonadIO m, MonadReader env m, HasLogFunc env)
  => FilePath
  -> IHashTable ShortText TMParameterDef
  -> IHashTable Int VarParams
  -> m (Either Text (Warnings, IHashTable TMPacketKey TMPacketDef))
loadPackets mibPath parameters vpdLookup = do
  runExceptT $ do
    -- load calibrations
    pids' <- PID.loadFromFile mibPath
    tpcfs' <- TPCF.loadFromFile mibPath
    plfs' <- PLF.loadFromFile mibPath

    let pid = fromRight V.empty pids'
        tpcf = fromRight V.empty tpcfs'
        plf = fromRight V.empty plfs'
        tpcfMap = TPCF.getTPCFMap tpcf

    (warnings, packets) <- liftEither $ convertPackets tpcfMap plf vpdLookup parameters pid
    let key pkt = TMPacketKey (_tmpdApid pkt)
            (_tmpdType pkt) (_tmpdSubType pkt)
            (fromIntegral (_tmpdPI1Val pkt)) (fromIntegral (_tmpdPI2Val pkt))
        lst = map (\x -> (key x, x)) (fixedTMPacketDefs ++ packets)
    hm <- liftEither $ runST $ do
      ht <- HTC.fromList lst
      Right <$> HT.unsafeFreeze ht

    return (warnings, hm)
