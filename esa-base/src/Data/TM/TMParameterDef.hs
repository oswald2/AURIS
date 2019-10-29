
{-|
Module      : Data.TM.TMParameterDef
Description : The definition of a TM parameter
Copyright   : (c) Michael Oswald, 2019
License     : BSD-3
Maintainer  : michael.oswald@onikudaki.net
Stability   : experimental
Portability : POSIX

This module is for the TM parameter definition, i.e. for handling
the information read out of a SCOS MIB, EGS-CC CDM etc.
-}
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
    , TemplateHaskell
#-}
module Data.TM.TMParameterDef
    ( DoubleType(..)
    , TimeType(..)
    , ParamType(..)
    , ParamNatur(..)
    , TMParameterDef(..)
    , StatusConsistency(..)
    , ptcPfcToParamType
    , charToStatusConsistency
    , fpName
    , fpDescription
    , fpPID
    , fpUnit
    , fpType
    , fpWidth
    , fpValid
    , fpRelated
    , fpCalibs
    , fpNatur
    , fpInterpolation
    , fpStatusConsistency
    , fpDecim
    , fpDefaultVal
    , fpSubsys
    , fpValidityValue
    , fpOBTID
    , fpEndian
    )
where


import           RIO
import           Control.Lens                   ( makeLenses )
import           Data.Text.Short                ( ShortText )
import           Codec.Serialise
import           Data.Aeson

import           Data.TM.Value
import           Data.TM.Calibration
import           Data.TM.CalibrationTypes
import           Data.TM.Synthetic

import           General.Types
import           General.PUSTypes

-- | If a value is a double, describes which double type
-- is used.
data DoubleType =
    -- | Normal double value
    DTDouble
    -- | Single precision float value
    | DTFloat
    -- | MIL-1750 single precision floating point (32 bit)
    | DTMilSingle
    -- | MIL-1750 extended precision floating point (48 bit)
    | DTMilExtended
    deriving (Eq, Ord, Enum, Bounded, Show, Generic)

instance NFData DoubleType
instance Serialise DoubleType
instance FromJSON DoubleType
instance ToJSON DoubleType where
    toEncoding = genericToEncoding defaultOptions

-- | Specifies the time format encoding. Currently, only a
-- subset of the valid time values are supported
data TimeType =
    CDS6 Bool
    | CDS8 Bool
    | CUC4Coarse2Fine Bool
    deriving (Eq, Ord, Show, Generic)

instance NFData TimeType
instance Serialise TimeType
instance FromJSON TimeType
instance ToJSON TimeType where
    toEncoding = genericToEncoding defaultOptions


-- | Specifies, if the parameter is a time value, if
-- this value should go through the time correlation
data CorrelationType = CorrelationYes | CorrelationNo
    deriving (Eq, Ord, Enum, Bounded, Show, Generic)

instance NFData CorrelationType
instance Serialise CorrelationType
instance FromJSON CorrelationType
instance ToJSON CorrelationType where
    toEncoding = genericToEncoding defaultOptions


-- | Specifies the data type of the parameter
data ParamType =
    -- | the parameter is a signed integer in the given width
    ParamInteger !Int
    -- | the parameter is an unsigned integer in the given width
    | ParamUInteger !Int
    -- | the parameter is a floating point value with the given 'DoubleType'
    | ParamDouble !DoubleType
    -- | the parameter is a time value. The 'TimeType' specifies which encoding
    -- format is used, the 'CorrelationType' if value of this parameter should
    -- be put through the time correlation
    | ParamTime !TimeType !CorrelationType
    -- | the parameter is a (ASCCI)-string. Though the system generally supports
    -- UTF-8, when a MIB is used only ASCII is allowed. In case of Nothing, the
    -- string is variable length, or Just a number of bytes long
    | ParamString (Maybe Int)
    -- | the parameter is an octet-string (so a 'ByteString'). In case of Nothing, the
    -- string is variable length, or Just a number of bytes long
    | ParamOctet (Maybe Int)
    -- | The parameter is a deduced parameter (not implemented yet)
    | ParamDeduced (Maybe Int)
    -- | The parameter is a saved synthetic parameter. Note that synthetic parameters
    -- in AURIS are always saved synthetic, hence this type has no effect
    | ParamSavedSynthetic
    deriving (Show, Generic)


-- | Converts from a 'PTC' and a 'PFC' value (according to the SCOS-2000 MIB ICD 6.9)
-- into a 'ParamType'. The Maybe Bool parameter is only applicable on time parameters
-- and specifies if a calibration should be used when extracting the time value out of
-- a packet.
-- Returns an error message, when no result can be provided or the 'ParamType'.
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
ptcPfcToParamType (PTC 5) (PFC 4 ) _ = Right $ ParamDouble DTMilExtended
ptcPfcToParamType (PTC 6) (PFC 0 ) _ = Left "Variable bit string not supported"
ptcPfcToParamType (PTC 6) (PFC x ) _ = Right $ ParamUInteger x
ptcPfcToParamType (PTC 7) (PFC 0 ) _ = Right $ ParamOctet Nothing
ptcPfcToParamType (PTC 7) (PFC x ) _ = Right $ ParamOctet (Just x)
ptcPfcToParamType (PTC 8) (PFC 0 ) _ = Right $ ParamString Nothing
ptcPfcToParamType (PTC 8) (PFC x ) _ = Right $ ParamString (Just x)
ptcPfcToParamType (PTC 9) (PFC 0 ) _ = Left "PTC=9 PFC=0 not supported"
ptcPfcToParamType (PTC 9) (PFC 1) Nothing =
    Right $ ParamTime (CDS6 False) CorrelationYes
ptcPfcToParamType (PTC 9) (PFC 1) (Just True) =
    Right $ ParamTime (CDS6 False) CorrelationYes
ptcPfcToParamType (PTC 9) (PFC 1) (Just False) =
    Right $ ParamTime (CDS6 False) CorrelationNo
ptcPfcToParamType (PTC 9) (PFC 2) Nothing =
    Right $ ParamTime (CDS8 False) CorrelationYes
ptcPfcToParamType (PTC 9) (PFC 2) (Just True) =
    Right $ ParamTime (CDS8 False) CorrelationYes
ptcPfcToParamType (PTC 9) (PFC 2) (Just False) =
    Right $ ParamTime (CDS8 False) CorrelationNo
ptcPfcToParamType (PTC 9) (PFC 17) Nothing =
    Right $ ParamTime (CUC4Coarse2Fine False) CorrelationYes
ptcPfcToParamType (PTC 9) (PFC 17) (Just True) =
    Right $ ParamTime (CUC4Coarse2Fine False) CorrelationYes
ptcPfcToParamType (PTC 9) (PFC 17) (Just False) =
    Right $ ParamTime (CUC4Coarse2Fine False) CorrelationNo
ptcPfcToParamType (PTC 10) (PFC 17) _ =
    Right $ ParamTime (CUC4Coarse2Fine True) CorrelationNo
ptcPfcToParamType (PTC 11) (PFC 0) _ = Right $ ParamDeduced Nothing
ptcPfcToParamType (PTC 11) (PFC x) _ = Right $ ParamDeduced (Just x)
ptcPfcToParamType (PTC 13) (PFC 0) _ = Right ParamSavedSynthetic
ptcPfcToParamType ptc pfc _ =
    Left $ "Unsupported: " <> textDisplay ptc <> " " <> textDisplay pfc



instance NFData ParamType
instance Serialise ParamType
instance FromJSON ParamType
instance ToJSON ParamType where
    toEncoding = genericToEncoding defaultOptions

-- | The parameter nature.
data ParamNatur =
    -- | A normal parameter
    NaturRaw
    -- | A synthetic parameter
    | NaturSynthetic { _pnSynth :: Synthetic }
    -- | A user-defined constant parameter
    | NaturConstant
    deriving (Show, Generic)

instance NFData ParamNatur
instance Serialise ParamNatur
instance FromJSON ParamNatur
instance ToJSON ParamNatur where
    toEncoding = genericToEncoding defaultOptions

-- | Defines if the parameter is status consistency checked
data StatusConsistency = SCCOn | SCCOff
    deriving (Eq, Ord, Enum, Bounded, Show, Generic)


-- | Convert from a Char as given in the SCOS-2000 MIB ICD 6.9
-- into a 'StatusConsistency' value
charToStatusConsistency :: Char -> StatusConsistency
charToStatusConsistency 'Y' = SCCOn
charToStatusConsistency _   = SCCOff

instance NFData StatusConsistency
instance Serialise StatusConsistency
instance FromJSON StatusConsistency
instance ToJSON StatusConsistency where
    toEncoding = genericToEncoding defaultOptions


-- | The whole parameter definition
data TMParameterDef = TMParameterDef {
    -- | The parameter name
    _fpName :: !ShortText
    -- | a description
    , _fpDescription :: !ShortText
    -- | if a parameter has a PID (parameter identification) value. This is
    -- not to be confused with a PID in the sense of APID values.
    , _fpPID :: Maybe Word32
    -- | a unit for the parameter (eg "sec", "m", "kg")
    , _fpUnit :: !ShortText
    -- | the param type
    , _fpType :: !ParamType
    -- | the padding width. This is not the width of the parameter, which is
    -- given in the '_fpType' field, but if the parameter should have additional
    -- padding to the next one
    , _fpWidth :: Maybe Word32
    -- | Reference to the validity parameter. If the validity parameter has a value
    -- equal to '_fpValidityValue', the validity to set to true for this parameter
    , _fpValid :: Maybe TMParameterDef
    -- | Related. Currently not used
    , _fpRelated :: Maybe TMParameterDef
    -- | the calibrations assigned to this parameter
    , _fpCalibs :: CalibContainer
    -- | the nature of the parameter (see 'ParamNatur')
    , _fpNatur :: !ParamNatur
    -- | specifies how out-of-range calibrations should be handled
    , _fpInterpolation :: !CalibInterpolation
    -- | specifies if the parameter is status consistency checked
    , _fpStatusConsistency :: !StatusConsistency
    -- | the number of decimal digits to be used when displaying the parameter.
    -- Not used within AURIS
    , _fpDecim :: !Int
    -- | default value. Only used when the parameter is a constant parameter
    , _fpDefaultVal :: !TMValue
    -- | the subsystem this parameter is assigned to
    , _fpSubsys :: !ShortText
    -- | the value the validity parameter from '_fpValid' will be checked against
    , _fpValidityValue :: !TMValue
    -- | the on-board clock ID to be used for correlation
    , _fpOBTID :: Maybe Int
    -- | the endianess of the parameter. Only applicable for whole-byte sized
    -- parameters
    , _fpEndian :: Endian
    }
    deriving(Show, Generic)
makeLenses ''TMParameterDef


instance NFData TMParameterDef
instance Serialise TMParameterDef
instance FromJSON TMParameterDef
instance ToJSON TMParameterDef where
    toEncoding = genericToEncoding defaultOptions

