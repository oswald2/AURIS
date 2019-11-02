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
  , dmTMPackets
  , dmPacketIdIdx
  , empty
  )
where

import           RIO
import qualified RIO.HashMap                   as HM
import           Control.Lens                   ( makeLenses )

import           Data.Text.Short                ( ShortText )

import           Codec.Serialise
import           Codec.Serialise.Encoding
import           Codec.Serialise.Decoding

import           Data.HashTable.ST.Basic       as HT

import           Data.TM.Calibration
import           Data.TM.Synthetic
import           Data.TM.TMParameterDef
import           Data.TM.TMPacketDef

import           General.Types




-- | The data model itself.
data DataModel = DataModel {
    -- | A map of the defined calibrations indexed by name
    _dmCalibrations :: HashMap ShortText Calibration
    -- | A map of the defined synthetic parameters indexed by name
    , _dmSyntheticParams :: HashMap ShortText Synthetic
    -- | A map of the defined TM parameters indexec by name
    , _dmParameters :: IHashTable ShortText TMParameterDef
    -- | A search index for packet identification criterias
    , _dmPacketIdIdx :: PICSearchIndex
    -- | A map into the packets
    , _dmTMPackets :: IHashTable TMPacketKey TMPacketDef
    }
    deriving (Show, Generic)
makeLenses ''DataModel


empty :: DataModel
empty =
  let (params, packets) = runST $ do
        prm  <- new
        pkts <- new
        (,) <$> unsafeFreeze prm <*> unsafeFreeze pkts
  in  DataModel { _dmCalibrations    = HM.empty
                , _dmSyntheticParams = HM.empty
                , _dmParameters      = params
                , _dmPacketIdIdx     = emptyPICSearchIndex
                , _dmTMPackets       = packets
                }


instance Serialise DataModel where
  encode = encodeDataModel
  decode = decodeDataModel

encodeDataModel :: DataModel -> Encoding
encodeDataModel model =
  encodeListLen 5 
    <> encode (_dmCalibrations model)
    <> encode (_dmSyntheticParams model)
    <> encodeHashTable (_dmParameters model)
    <> encode (_dmPacketIdIdx model)
    <> encodeHashTable (_dmTMPackets model)

decodeDataModel :: Decoder s DataModel 
decodeDataModel = do 
  _len <- decodeListLen 
  calibs <- decode 
  synths <- decode 
  params <- decodeHashTable 
  idx <- decode 
  packets <- decodeHashTable 
  return DataModel {
    _dmCalibrations = calibs 
    , _dmSyntheticParams = synths 
    , _dmParameters = params 
    , _dmPacketIdIdx = idx 
    , _dmTMPackets = packets
  }

