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
    , TemplateHaskell
#-}
module Data.DataModel
    ( DataModel(..)
    , dmCalibrations
    , dmSyntheticParams
    , dmParameters
    )
where

import           RIO

import           Control.Lens                   ( makeLenses )

import           Data.Text.Short                ( ShortText )

import           Data.TM.Calibration
import           Data.TM.Synthetic
import           Data.TM.TMParameterDef


data DataModel = DataModel {
    _dmCalibrations :: HashMap ShortText Calibration
    , _dmSyntheticParams :: HashMap ShortText Synthetic
    , _dmParameters :: HashMap ShortText TMParameterDef
    }
    deriving (Show, Generic)
makeLenses ''DataModel

