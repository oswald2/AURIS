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

import           Control.Monad.Trans.Except

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

import           Data.Conversion.Calibration





-- | load the whole MIB into a data structure
loadMIB
  :: (MonadIO m, MonadReader env m, HasLogFunc env)
  => FilePath
  -> m (Either Text MIB)
loadMIB mibPath = do
  runExceptT $ do
    calibs <- loadCalibs mibPath

    let mib = MIB {
        _mibCalibrations = fromRight HM.empty calibs 
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


    numCalibs' <- except $ traverse (`convertNumCalib` cap) caf
    let !numCalibs =
          HM.fromList
            . map (\x -> (_calibNName x, CalibNum x))
            . toList
            $ numCalibs'

    polyCalibs' <- except $ traverse convertPolyCalib mcf
    let !polyCalibs = V.foldl'
          (\hm x -> HM.insert (_calibPName x) (CalibPoly x) hm)
          numCalibs
          polyCalibs'

    logCalibs' <- except $ traverse convertLogCalib lgf
    let !logCalibs = V.foldl'
          (\hm x -> HM.insert (_calibLName x) (CalibLog x) hm)
          polyCalibs
          logCalibs'


    return logCalibs
