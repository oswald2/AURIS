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
    ( PUSMissionSpecific(..)
    , pmsTCDataFieldHeader
    , pmsTMDataFieldHeader
    , pmsTMFrameDataFieldHeader
    , pmsEpoch
    )
where

import           RIO

import           Control.Lens                   ( makeLenses )

import           Data.PUS.PUSDfh
import           Data.PUS.TMFrameDfh

import           General.Time


-- | A data type to customize for mission specific features. A default
-- implementation with PUS standard default values is provided
data PUSMissionSpecific = PUSMissionSpecific {
    -- | The data field header of a TC. Default is 'PUSTCStdHeader'
    _pmsTCDataFieldHeader :: DataFieldHeader
    -- | The data field header for TM packets. Default is 'PUSTMStdHeader'
    , _pmsTMDataFieldHeader :: DataFieldHeader
    -- | Indicates if TM Frames have a standard header and if yes, which one.
    -- Default is Nothing (no DFH for TM Frames)
    , _pmsTMFrameDataFieldHeader :: Maybe TMFrameDataFieldHeader
    -- | The time epoch the mission runs in
    , _pmsEpoch :: Epoch
} deriving (Show, Generic)
makeLenses ''PUSMissionSpecific

