
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
    | ParamDeduced (Maybe Int)
    | ParamSavedSynthetic
    deriving (Show, Generic)

ptcPfcToParamType :: PTC -> PFC -> Maybe Bool -> Either Text ParamType
ptcPfcToParamType (PTC 1) (PFC 0 ) _ = Right $ ParamUInteger 1
ptcPfcToParamType (PTC 2) (PFC x ) _ = Right $ ParamUInteger x
ptcPfcToParamType (PTC 3) (PFC 0 ) _ = Right $ ParamUInteger 4
ptcPfcToParamType (PTC 3) (PFC 1 ) _ = Right $ ParamUInteger 5
ptcPfcToParamType (PTC 3) (PFC 2 ) _ = Right $ ParamUInteger 6
ptcPfcToParamType (PTC 3) (PFC 3 ) _ = Right $ ParamUInteger 7
ptcPfcToParamType (PTC 3) (PFC 4 ) _ = Right $ ParamUInteger 8
ptcPfcToParamType (PTC 3) (PFC 5 ) _ = Right $ ParamUInteger 9
ptcPfcToParamType (PTC 3) (PFC 6 ) _ = Right $ ParamUInteger 10
ptcPfcToParamType (PTC 3) (PFC 7 ) _ = Right $ ParamUInteger 11
ptcPfcToParamType (PTC 3) (PFC 8 ) _ = Right $ ParamUInteger 12
ptcPfcToParamType (PTC 3) (PFC 9 ) _ = Right $ ParamUInteger 13
ptcPfcToParamType (PTC 3) (PFC 10) _ = Right $ ParamUInteger 14
ptcPfcToParamType (PTC 3) (PFC 11) _ = Right $ ParamUInteger 15
ptcPfcToParamType (PTC 3) (PFC 12) _ = Right $ ParamUInteger 16
ptcPfcToParamType (PTC 3) (PFC 13) _ = Right $ ParamUInteger 24
ptcPfcToParamType (PTC 3) (PFC 14) _ = Right $ ParamUInteger 32
ptcPfcToParamType (PTC 3) (PFC 15) _ = Right $ ParamUInteger 48
ptcPfcToParamType (PTC 3) (PFC 16) _ = Right $ ParamUInteger 64
ptcPfcToParamType (PTC 4) (PFC 0 ) _ = Right $ ParamInteger 4
ptcPfcToParamType (PTC 4) (PFC 1 ) _ = Right $ ParamInteger 5
ptcPfcToParamType (PTC 4) (PFC 2 ) _ = Right $ ParamInteger 6
ptcPfcToParamType (PTC 4) (PFC 3 ) _ = Right $ ParamInteger 7
ptcPfcToParamType (PTC 4) (PFC 4 ) _ = Right $ ParamInteger 8
ptcPfcToParamType (PTC 4) (PFC 5 ) _ = Right $ ParamInteger 9
ptcPfcToParamType (PTC 4) (PFC 6 ) _ = Right $ ParamInteger 10
ptcPfcToParamType (PTC 4) (PFC 7 ) _ = Right $ ParamInteger 11
ptcPfcToParamType (PTC 4) (PFC 8 ) _ = Right $ ParamInteger 12
ptcPfcToParamType (PTC 4) (PFC 9 ) _ = Right $ ParamInteger 13
ptcPfcToParamType (PTC 4) (PFC 10) _ = Right $ ParamInteger 14
ptcPfcToParamType (PTC 4) (PFC 11) _ = Right $ ParamInteger 15
ptcPfcToParamType (PTC 4) (PFC 12) _ = Right $ ParamInteger 16
ptcPfcToParamType (PTC 4) (PFC 13) _ = Right $ ParamInteger 24
ptcPfcToParamType (PTC 4) (PFC 14) _ = Right $ ParamInteger 32
ptcPfcToParamType (PTC 4) (PFC 15) _ = Right $ ParamInteger 48
ptcPfcToParamType (PTC 4) (PFC 16) _ = Right $ ParamInteger 64
ptcPfcToParamType (PTC 5) (PFC 1 ) _ = Right $ ParamDouble DTFloat
ptcPfcToParamType (PTC 5) (PFC 2 ) _ = Right $ ParamDouble DTDouble
ptcPfcToParamType (PTC 5) (PFC 3 ) _ = Right $ ParamDouble DTMilSingle
ptcPfcToParamType (PTC 5) (PFC 4 ) _ = Right $ ParamDouble DTMMilExtended
ptcPfcToParamType (PTC 6) (PFC 0 ) _ = Left $ "Variable bit string not supported"
ptcPfcToParamType (PTC 6) (PFC x ) _ = Right $ ParamUInteger x
ptcPfcToParamType (PTC 7) (PFC 0 ) _ = Right $ ParamOctet Nothing
ptcPfcToParamType (PTC 7) (PFC x ) _ = Right $ ParamOctet (Just x)
ptcPfcToParamType (PTC 8) (PFC 0 ) _ = Right $ ParamString Nothing
ptcPfcToParamType (PTC 8) (PFC x ) _ = Right $ ParamString (Just x)
ptcPfcToParamType (PTC 9) (PFC 0 ) _ = Left $ "PTC=9 PFC=0 not supported"
ptcPfcToParamType (PTC 9) (PFC 1 ) Nothing = Right $ ParamTime CDS6 CorrelationYes
ptcPfcToParamType (PTC 9) (PFC 1 ) (Just True) = Right $ ParamTime CDS6 CorrelationYes
ptcPfcToParamType (PTC 9) (PFC 1 ) (Just False) = Right $ ParamTime CDS6 CorrelationNo
ptcPfcToParamType (PTC 9) (PFC 2 ) Nothing = Right $ ParamTime CDS8 CorrelationYes
ptcPfcToParamType (PTC 9) (PFC 2 ) (Just True) = Right $ ParamTime CDS8 CorrelationYes
ptcPfcToParamType (PTC 9) (PFC 2 ) (Just False) = Right $ ParamTime CDS8 CorrelationNo
ptcPfcToParamType (PTC 9) (PFC 17) Nothing = Right $ ParamTime CUC4Coarse2Fine CorrelationYes
ptcPfcToParamType (PTC 9) (PFC 17) (Just True) = Right $ ParamTime CUC4Coarse2Fine CorrelationYes
ptcPfcToParamType (PTC 9) (PFC 17) (Just False) = Right $ ParamTime CUC4Coarse2Fine CorrelationNo
ptcPfcToParamType (PTC 10) (PFC 17) _ = Right $ ParamTime CUC4Coarse2Fine CorrelationNo
ptcPfcToParamType (PTC 11) (PFC 0) _ = Right $ ParamDeduced Nothing 
ptcPfcToParamType (PTC 11) (PFC x) _ = Right $ ParamDeduced (Just x)
ptcPfcToParamType (PTC 13) (PFC 0) _ = Right $ ParamSavedSynthetic 
ptcPfcToParamType ptc pfc _ = Left $ "Unsupported: " <> textDisplay ptc <> " " <> textDisplay pfc











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

