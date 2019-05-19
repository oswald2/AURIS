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
module Data.PUS.MissionSpecific.Definitions
(
    PUSMissionSpecific(..)
    , defaultMissionSpecific
    , pmsTCDataFieldHeader
    , pmsTMDataFieldHeader

)
where

import RIO

import Control.Lens (makeLenses)

import Data.PUS.PUSDfh
import Data.PUS.EncTime


-- | A data type to customize for mission specific features. A default
-- implementation with PUS standard default values is provided
data PUSMissionSpecific = PUSMissionSpecific {
    _pmsTCDataFieldHeader :: DataFieldHeader
    , _pmsTMDataFieldHeader :: DataFieldHeader
}
makeLenses ''PUSMissionSpecific

-- | a default value for PUS compliant missions
defaultMissionSpecific :: PUSMissionSpecific
defaultMissionSpecific = PUSMissionSpecific {
    _pmsTCDataFieldHeader = PUSTCStdHeader 0 0 0 True True False True
, _pmsTMDataFieldHeader = PUSTMStdHeader 0 0 0 0 nullCUCTime
}