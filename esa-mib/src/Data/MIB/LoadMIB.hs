{-# LANGUAGE
    AutoDeriveTypeable
    , BangPatterns
    , BinaryLiterals
    , ConstraintKinds
    , DataKinds
    , DefaultSignatures
    , DeriveDataTypeable
    , DeriveFoldable
    , DeriveFunctor
    , DeriveGeneric
    , DeriveTraversable
    , DoAndIfThenElse
    , EmptyDataDecls
    , ExistentialQuantification
    , FlexibleContexts
    , FlexibleInstances
    , FunctionalDependencies
    , GADTs
    , GeneralizedNewtypeDeriving
    , InstanceSigs
    , KindSignatures
    , LambdaCase
    , MonadFailDesugaring
    , MultiParamTypeClasses
    , MultiWayIf
    , NamedFieldPuns
    , NoImplicitPrelude
    , OverloadedStrings
    , PartialTypeSignatures
    , PatternGuards
    , PolyKinds
    , RankNTypes
    , RecordWildCards
    , ScopedTypeVariables
    , StandaloneDeriving
    , TupleSections
    , TypeFamilies
    , TypeSynonymInstances
    , ViewPatterns
#-}
module Data.MIB.LoadMIB
    ( loadMIB
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

import           Data.Either
import           Data.Text.Short                ( ShortText )
import qualified Data.MIB.CAF                  as CAF
import qualified Data.MIB.CAP                  as CAP
import qualified Data.MIB.MCF                  as MCF
import qualified Data.MIB.LGF                  as LGF
import qualified Data.MIB.TXP                  as TXP
import qualified Data.MIB.TXF                  as TXF

import           Data.MIB.MIB

import           Data.TM.Calibration
import           Data.TM.NumericalCalibration
import           Data.TM.PolynomialCalibration
import           Data.TM.LogarithmicCalibration
import           Data.TM.TextualCalibration
import           Data.TM.Synthetic

import           Data.Conversion.Calibration





-- | load the whole MIB into a data structure
loadMIB
    :: (MonadUnliftIO m, MonadReader env m, HasLogFunc env)
    => FilePath
    -> m (Either Text MIB)
loadMIB mibPath = do
    syns' <- loadSyntheticParameters mibPath
    case syns' of
        Left  err  -> return (Left err)
        Right syns -> runExceptT $ do
            calibs <- loadCalibs mibPath
            let mib = MIB { _mibCalibrations    = fromRight HM.empty calibs
                          , _mibSyntheticParams = syns
                          }
            return mib


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
    :: (MonadUnliftIO m)
    => FilePath
    -> m (Either Text (HashMap ShortText Synthetic))
loadSyntheticParameters path = do
    handleIO
            (\exc -> return
                (Left
                    (  "Error reading synthetic parameters: "
                    <> T.pack (displayException exc)
                    )
                )
            )
        $   doesDirectoryExist path
        >>= \x -> if not x
                then
                    do
                        pure
                    $  Left
                    $  "Could not read synthetic parameters: directory '"
                    <> T.pack path
                    <> "' does not exist"
                else do
                    files <- listDirectory path >>= filterM doesFileExist
                    ols   <- forM (map (path </>) files) parseOL
                    if all isRight ols
                        then do
                            let syn = zipWith f files (rights ols)
                                f p ol = (fromString p, ol)
                                !hm = HM.fromList syn
                            return (Right hm)
                        else
                            do
                                return
                            $  Left
                            $  T.concat
                            $  ["Error parsing synthetic parameters: " :: Text]
                            <> intersperse "\n" (lefts ols)

