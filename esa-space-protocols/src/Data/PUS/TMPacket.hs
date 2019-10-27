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
module Data.PUS.TMPacket
  ( TMPacket(..)
  )
where


import           RIO

import           Codec.Serialise
import           Data.Aeson

import           General.PUSTypes
import           General.APID

import           Data.TM.Parameter
--import           Data.PUS.Value

--import           General.Types
import           General.Time


data TMPacket = TMPacket {
    _tmpSPID :: SPID
    , _tmpAPID :: APID
    , _tmpType :: PUSType
    , _tmpSubType :: PUSSubType
    , _tmpERT :: SunTime
    , _tmpTimeStamp :: SunTime
    , _tmpVCID :: VCID
    , _tmpParams :: Vector TMParameter
    } deriving (Show, Generic)

instance Serialise TMPacket
instance FromJSON TMPacket
instance ToJSON TMPacket where
  toEncoding = genericToEncoding defaultOptions
