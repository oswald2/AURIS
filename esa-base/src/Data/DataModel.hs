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
    , dmGRDs
    , dmInfo
    , dmTCs
    , dmTCParameters
    , empty
    , writeDataModel
    , readDataModel
    , dataModelFindParam
    ) where

import           Control.Lens                   ( makeLenses )
import           RIO
import qualified RIO.HashMap                   as HM
import qualified RIO.Map                       as M
--import qualified RIO.ByteString.Lazy           as BL
import qualified RIO.Text                      as T

import           Codec.Serialise               as S
import           Codec.Serialise.Decoding      as S
import           Codec.Serialise.Encoding      as S
import           Data.Aeson                    as AE
import           Data.Text.Short                ( ShortText )

import           Data.HashTable.ST.Basic       as HT

import           Data.PUS.DataModelInfo
import           Data.TM.Calibration
import           Data.TM.Synthetic
import           Data.TM.TMPacketDef
import           Data.TM.TMParameterDef

import           Data.TC.TCDef
import           Data.TC.TCParameterDef

import           Data.Display.Graphical

import           General.Types



-- | The data model itself.
data DataModel = DataModel
    { _dmInfo            :: DataModelInfo
    -- | A map of the defined calibrations indexed by name
    , _dmCalibrations    :: HashMap ShortText Calibration
    -- | A map of the defined synthetic parameters indexed by name
    , _dmSyntheticParams :: HashMap ShortText Synthetic
    -- | A map of the defined TM parameters indexec by name
    , _dmParameters      :: IHashTable ShortText TMParameterDef
    -- | A search index for packet identification criterias
    , _dmPacketIdIdx     :: PICSearchIndex
    -- | Graphical displays 
    , _dmGRDs            :: Map ShortText GRD
    -- | A map into the packets
    , _dmTMPackets       :: IHashTable TMPacketKey TMPacketDef
    -- | A map into VPD structures. Used for VPD_CHOICE variable packets.
    -- They get a value, which is interpreted as a TPSD value, which is looked
    -- up in this table. The rest of the packet is then replaced with these 
    -- 'VarParams' structure. 
    , _dmVPDStructs      :: IHashTable Int VarParams
    -- | A hash table for the TC Commands available in the MIB
    , _dmTCs             :: IHashTable ShortText TCDef
    -- | A hash map of all TC parameters available in the MIB
    , _dmTCParameters    :: HashMap ShortText TCParameterDef
    }
    deriving (Show, Generic)
makeLenses ''DataModel



dataModelFindParam :: DataModel -> ShortText -> Maybe TMParameterDef
dataModelFindParam model paramName = HT.ilookup (_dmParameters model) paramName

-- instance Show (Compact DataModel) where
--     show x = show (getCompact x)

-- | returns an empty data model. 
empty :: DataModel
empty =
    let (params, packets) = runST $ do
            prm  <- new
            pkts <- new
            (,) <$> unsafeFreeze prm <*> unsafeFreeze pkts
    in  DataModel { _dmInfo            = defaultDataModelInfo
                  , _dmCalibrations    = HM.empty
                  , _dmSyntheticParams = HM.empty
                  , _dmParameters      = params
                  , _dmPacketIdIdx     = emptyPICSearchIndex
                  , _dmTMPackets       = packets
                  , _dmVPDStructs      = HT.iempty
                  , _dmGRDs            = M.empty
                  , _dmTCs             = HT.iempty
                  , _dmTCParameters    = HM.empty
                  }


instance Serialise DataModel where
    encode = encodeDataModel
    decode = decodeDataModel

instance FromJSON DataModel
instance ToJSON DataModel where
    toEncoding = genericToEncoding defaultOptions



encodedLen :: Word
encodedLen = 10

encodeDataModel :: DataModel -> S.Encoding
encodeDataModel model =
    encodeListLen encodedLen
        <> S.encode (_dmInfo model)
        <> S.encode (_dmCalibrations model)
        <> S.encode (_dmSyntheticParams model)
        <> S.encode (_dmGRDs model)
        <> S.encode (_dmPacketIdIdx model)
        <> encodeHashTable (_dmParameters model)
        <> encodeHashTable (_dmTMPackets model)
        <> encodeHashTable (_dmVPDStructs model)
        <> encodeHashTable (_dmTCs model)
        <> S.encode (_dmTCParameters model)

decodeDataModel :: Decoder s DataModel
decodeDataModel = do
    len <- decodeListLen
    when (fromIntegral len /= encodedLen)
        $  fail
        $  "Error decoding data model: "
        <> show len
        <> ", should be "
        <> show encodedLen
    info     <- S.decode
    calibs   <- S.decode
    synths   <- S.decode
    grds     <- S.decode
    idx      <- S.decode
    params   <- decodeHashTable
    packets  <- decodeHashTable
    vpds     <- decodeHashTable
    tcs      <- decodeHashTable
    tcParams <- S.decode
    return DataModel { _dmInfo            = info
                     , _dmCalibrations    = calibs
                     , _dmSyntheticParams = synths
                     , _dmPacketIdIdx     = idx
                     , _dmGRDs            = grds
                     , _dmParameters      = params
                     , _dmTMPackets       = packets
                     , _dmVPDStructs      = vpds
                     , _dmTCs             = tcs
                     , _dmTCParameters    = tcParams
                     }

-- | Serializes the 'DataModel' and writes it to a file. Uses 
-- the serialise library under the hood.
writeDataModel :: (MonadIO m) => FilePath -> DataModel -> m ()
writeDataModel path model = liftIO $ writeFileSerialise path model

-- | Reads the serialized 'DataModel' from a file
readDataModel :: (MonadUnliftIO m) => FilePath -> m (Either Text DataModel)
readDataModel path = do
    res <- try $ liftIO $ readFileDeserialise path
    case res of
        Left  (DeserialiseFailure _ err) -> return $ Left (T.pack err)
        Right model                      -> return (Right model)


