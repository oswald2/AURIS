
{-# LANGUAGE OverloadedStrings
    , BangPatterns
    , GeneralizedNewtypeDeriving
    , DeriveGeneric
    , RecordWildCards
    , NoImplicitPrelude
    , BinaryLiterals
    , NumericUnderscores
    , FlexibleInstances
    , GADTs
    , ExistentialQuantification
#-}
module Data.Model.TMParameterDef
  ( DoubleType(..)
  , TimeType(..)
  , ParamType(..)
  , TMParameterDef(..)
  )
where


import           RIO

import           Data.Text.Short                ( ShortText )

import           Data.Model.CalibrationDef

import           Data.TM.Value
import           General.Types


data DoubleType =
    DTDouble
    | DTFloat
    | DTMilSingle
    | DTMMilExtended
    deriving (Eq, Ord, Enum, Bounded, Show, Generic)

data TimeType =
    CDS6
    | CDS8
    | CUC4Coarse2Fine
    deriving (Eq, Ord, Enum, Bounded, Show, Generic)


data CorrelationType = CorrelationYes | CorrelationNo
    deriving (Eq, Ord, Enum, Bounded, Show, Generic)


data ParamType =
    ParamInteger Int
    | ParamUInteger Int
    | ParamDouble DoubleType
    | ParamTime TimeType CorrelationType
    | ParamString (Maybe Int)
    | ParamOctet (Maybe Int)
    deriving (Show, Generic)

data ParamNatur =
    NaturRaw
    | NaturSynthetic { _pnScript :: Text }
    | NaturConstant
    deriving (Show, Generic)

data InterpolationType =
    InterInterpolation
    | InterInvalid
    deriving (Show, Generic)

data StatusConsistency = SCCOn | SCCOff
    deriving (Eq, Ord, Enum, Bounded, Show, Generic)



data TMParameterDef = FixedParam {
    _fpName :: !ShortText
    , _fpDescription :: !ShortText
    , _fpPID :: !Word32
    , _fpUnit :: !ShortText
    , _fpType :: !ParamType
    , _fpWidth :: Maybe Word32
    , _fpValid :: Maybe TMParameterDef
    , _fpRelated :: Maybe TMParameterDef
    , _fpCalib :: !CalibrationDef
    , _fpCalibs :: [CalibrationDef]
    , _fpNatur :: !ParamNatur
    , _fpInterpolation :: !InterpolationType
    , _fpStatusConsistency :: !StatusConsistency
    , _fpDecim :: !Int
    , _fpDefaultVal :: !TMValue
    , _fpSubsys :: !ShortText
    , _fpValidityValue :: !TMValue
    , _fpOBTID :: Maybe Int
    , _fpEndian :: Endian
    }
    deriving(Show, Generic)

