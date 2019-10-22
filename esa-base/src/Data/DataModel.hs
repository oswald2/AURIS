{-|
Module      : Data.DataModel
Description : Represents the data model loaded from the MIB or CDM
Copyright   : (c) Michael Oswald, 2019
License     : BSD-3
Maintainer  : michael.oswald@onikudaki.net
Stability   : experimental
Portability : POSIX

This module provides the data structures for the data model. The information
from a MIB (mission information base) or a CDM (component data model, which is
currently not yet supported) is parsed and read in in the specific libraries
dedicated to the model (e.g. esa-mib for the MIB). The model is used by the TM model
and the TC model to describe the packets and commands used as well as some behaviours
like calibrations, limit checks, synthetic parameters and so on
-}
{-# LANGUAGE
    TemplateHaskell
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

import           Codec.Serialise

import           Data.HashTable.IO

import           Data.TM.Calibration
import           Data.TM.Synthetic
import           Data.TM.TMParameterDef


-- | The data model itself.
data DataModel = DataModel {
    -- | A map of the defined calibrations indexed by name
    _dmCalibrations :: HashMap ShortText Calibration
    -- | A map of the defined synthetic parameters indexed by name
    , _dmSyntheticParams :: HashMap ShortText Synthetic
    -- | A map of the defined TM parameters indexec by name
    , _dmParameters :: BasicHashTable ShortText TMParameterDef
    }
    deriving (Show, Generic)
makeLenses ''DataModel


