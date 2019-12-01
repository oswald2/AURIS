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
  , dmVPDStructs
  , empty
  , writeDataModel
  , readDataModel
  )
where

import           RIO
import qualified RIO.HashMap                   as HM
import qualified RIO.ByteString.Lazy           as BL
import qualified RIO.Text                      as T
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
    -- | A map into VPD structures. Used for VPD_CHOICE variable packets.
    -- They get a value, which is interpreted as a TPSD value, which is looked
    -- up in this table. The rest of the packet is then replaced with these 
    -- 'VarParams' structure. 
    , _dmVPDStructs :: IHashTable Int VarParams
    }
    deriving (Show, Generic)
makeLenses ''DataModel

-- | returns an empty data model. 
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
                , _dmVPDStructs      = HT.iempty
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
    <> encodeHashTable (_dmVPDStructs model)

decodeDataModel :: Decoder s DataModel
decodeDataModel = do
  _len    <- decodeListLen
  calibs  <- decode
  synths  <- decode
  params  <- decodeHashTable
  idx     <- decode
  packets <- decodeHashTable
  vpds    <- decodeHashTable 
  return DataModel { _dmCalibrations    = calibs
                   , _dmSyntheticParams = synths
                   , _dmParameters      = params
                   , _dmPacketIdIdx     = idx
                   , _dmTMPackets       = packets
                   , _dmVPDStructs      = vpds
                   }

-- | Serializes the 'DataModel' and writes it to a file. Uses 
-- the serialise library under the hood.
writeDataModel :: (MonadIO m) => FilePath -> DataModel -> m ()
writeDataModel path model = liftIO $ BL.writeFile path (serialise model)

-- | Reads the serialized 'DataModel' from a file
readDataModel :: (MonadIO m) => FilePath -> m (Either Text DataModel)
readDataModel path = do
  res <- deserialiseOrFail <$> BL.readFile path
  case res of
    Left  (DeserialiseFailure _ err) -> return $ Left (T.pack err)
    Right model                      -> return (Right model)


