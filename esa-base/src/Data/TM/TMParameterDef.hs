
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
module Data.TM.TMParameterDef
  ( DoubleType(..)
  , TimeType(..)
  , ParamType(..)
  , TMParameterDef(..)
  , ptcPfcToParamType
  )
where


import           RIO

import           Data.Text.Short                ( ShortText )
import           Codec.Serialise
import           Data.Aeson

import           Data.TM.Value
import           Data.TM.Calibration

import           General.Types
import           General.PUSTypes


data DoubleType =
    DTDouble
    | DTFloat
    | DTMilSingle
    | DTMMilExtended
    deriving (Eq, Ord, Enum, Bounded, Show, Generic)

instance Serialise DoubleType
instance FromJSON DoubleType
instance ToJSON DoubleType where
  toEncoding = genericToEncoding defaultOptions


data TimeType =
    CDS6
    | CDS8
    | CUC4Coarse2Fine
    deriving (Eq, Ord, Enum, Bounded, Show, Generic)

instance Serialise TimeType
instance FromJSON TimeType
instance ToJSON TimeType where
  toEncoding = genericToEncoding defaultOptions

data CorrelationType = CorrelationYes | CorrelationNo
    deriving (Eq, Ord, Enum, Bounded, Show, Generic)

instance Serialise CorrelationType
instance FromJSON CorrelationType
instance ToJSON CorrelationType where
  toEncoding = genericToEncoding defaultOptions

data ParamType =
    ParamInteger Int
    | ParamUInteger Int
    | ParamDouble DoubleType
    | ParamTime TimeType CorrelationType
    | ParamString (Maybe Int)
    | ParamOctet (Maybe Int)
    deriving (Show, Generic)

ptcPfcToParamType :: PTC -> PFC -> ParamType
ptcPfcToParamType (PTC 2) (PFC x) = ParamUInteger x


instance Serialise ParamType
instance FromJSON ParamType
instance ToJSON ParamType where
  toEncoding = genericToEncoding defaultOptions


data ParamNatur =
    NaturRaw
    | NaturSynthetic { _pnScript :: Text }
    | NaturConstant
    deriving (Show, Generic)

instance Serialise ParamNatur
instance FromJSON ParamNatur
instance ToJSON ParamNatur where
  toEncoding = genericToEncoding defaultOptions


data InterpolationType =
    InterInterpolation
    | InterInvalid
    deriving (Show, Generic)

instance Serialise InterpolationType
instance FromJSON InterpolationType
instance ToJSON InterpolationType where
  toEncoding = genericToEncoding defaultOptions


data StatusConsistency = SCCOn | SCCOff
    deriving (Eq, Ord, Enum, Bounded, Show, Generic)

instance Serialise StatusConsistency
instance FromJSON StatusConsistency
instance ToJSON StatusConsistency where
  toEncoding = genericToEncoding defaultOptions


data TMParameterDef = TMParameterDef {
    _fpName :: !ShortText
    , _fpDescription :: !ShortText
    , _fpPID :: !Word32
    , _fpUnit :: !ShortText
    , _fpType :: !ParamType
    , _fpWidth :: Maybe Word32
    , _fpValid :: Maybe TMParameterDef
    , _fpRelated :: Maybe TMParameterDef
    , _fpCalibs :: [Calibration]
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

instance Serialise TMParameterDef
instance FromJSON TMParameterDef
instance ToJSON TMParameterDef where
  toEncoding = genericToEncoding defaultOptions

