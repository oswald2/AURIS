{-|
Module      : Data.TM.TMPacketDef
Description : Data types for TM packet definitions
Copyright   : (c) Michael Oswald, 2019
License     : BSD-3
Maintainer  : michael.oswald@onikudaki.net
Stability   : experimental
Portability : POSIX

This module is very important, as it contains data types and functions for
the TM packet processing.

When a packet is received, it must be identified first. This is not a completely
trivial task. A packet is identified by the tuple (APID, Type, SubType, PI1 value, PI2 value).
The problem is, that PI1 and PI2 values can be anywhere in the packet and need
to be determined first.
Therefore first the packet identification criterias need to be queried to
determine the correct location (offset and bit-width) of the PIx values to
be extracted.

When the values are extracted, they can form the tuple to lookup the correct
TM packet definition which specifies how the packet looks like and how the
extraction should be done.
-}
{-# LANGUAGE
    TemplateHaskell
#-}
module Data.TM.TMPacketDef
  ( TMPacketDef(..)
  , SuperCommutated(..)
  , TMParamLocation(..)
  , TMVarParamDef(..)
  , TMVarParamModifier(..)
  , TMVarAlignment(..)
  , TMVarDisp(..)
  , TMVarRadix(..)
  , TMPacketParams(..)
  , PIDEvent(..)
  , tmpdSPID
  , tmpdType
  , tmpdSubType
  , tmpdApid
  , tmpdPI1Val
  , tmpdPI2Val
  , tmpdName
  , tmpdParams
  , tmpdDescr
  , tmpdUnit
  , tmpdTime
  , tmpdInter
  , tmpdValid
  , tmpdCheck
  , tmpdEvent
  , scNbOcc
  , scLgOcc
  , scTdOcc
  , tmplName
  , tmplOffset
  , tmplTime
  , tmplSuperComm
  , tmplParam
  , isSuperCommutated
  , TMPacketMap
  , TMPacketKey(..)
  , PICSearchIndex(..)
  , mkPICSearchIndex
  , emptyPICSearchIndex
  , ApidKey
  , TypeSubTypeKey
  , PacketIDCriteria(..)
  , picFind
  )
where

import           RIO
import qualified RIO.Vector                    as V
import           Data.Text.Short                ( ShortText )
import           Data.HashTable.ST.Basic        ( IHashTable )
import qualified Data.HashTable.ST.Basic       as HT
import           Control.Lens                   ( makeLenses )
import qualified Data.Aeson                    as AE

import           Codec.Serialise
import           Codec.Serialise.Encoding
import           Codec.Serialise.Decoding

import           General.PUSTypes
import           General.APID
import           General.Types
import           General.Time
import           General.TimeSpan

import           Data.TM.TMParameterDef
import           Data.TM.PIVals


-- | Specifies the super-commutated properties of a parameter.
data SuperCommutated = SuperCommutated {
  -- | The number of occurences of the paramter sequentially in the packet.
  -- Effectively, this parameter is repeated N times
  _scNbOcc :: !Int
  -- | Number of bits between the start of 2 consecutive supercommutated
  -- values (0..32676)
  , _scLgOcc :: !BitSize
  -- | Time delay in milliseconds between 2 consecutive supercommutated
  -- parameters (1 .. 4080000 ms)
  , _scTdOcc :: TimeSpn MilliSeconds
  } deriving (Show, Generic)
makeLenses ''SuperCommutated

instance Serialise SuperCommutated


-- | This data type specifies a parameter location and is therefore the
-- link between a packet and a parameter.
data TMParamLocation = TMParamLocation {
  -- | The name of the parameter
  _tmplName :: !ShortText
  -- | The bit-offset of the parameter in the packet
  , _tmplOffset :: !Offset
  -- | Time offset of the first parameter occurence relative to the packet
  -- time. This is effectively a delta time added/subtracted from the packet
  -- timestamp.
  , _tmplTime :: !SunTime
  -- | Specifies the super-commutated properties of this parameter, if any
  , _tmplSuperComm :: Maybe SuperCommutated
  -- | A link to the parameter definitions 'TMParameterDef'
  , _tmplParam :: TMParameterDef
  } deriving (Show, Generic)
makeLenses ''TMParamLocation

instance Serialise TMParamLocation

-- | returns if a param location is supercommutated
isSuperCommutated :: TMParamLocation -> Bool
isSuperCommutated TMParamLocation {..} = isJust _tmplSuperComm

-- | An event. It is possible to specify, that an event/alaram is raised
-- when a corresponding packet is received. This defines the severity of
-- the event raised (if any). If 'PIDNo' is specified, no event is raised (default)
data PIDEvent =
  PIDNo
  | PIDInfo !ShortText
  | PIDWarning !ShortText
  | PIDAlarm !ShortText
  deriving (Eq, Ord, Show, Generic)

instance Serialise PIDEvent
instance AE.FromJSON PIDEvent
instance AE.ToJSON PIDEvent where
  toEncoding = AE.genericToEncoding AE.defaultOptions



data TMVarParamModifier =
  TMVarNothing
  | TMVarGroup !Word16
  | TMVarFixedRep !Word16
  | TMVarChoice
  | TMVarPidRef
  deriving (Show, Generic)

instance Serialise TMVarParamModifier


data TMVarAlignment =
  TMVarLeft
  | TMVarRight
  | TMVarCenter
  deriving (Show, Generic)

instance Serialise TMVarAlignment


data TMVarDisp =
  TMVarDispValue
  | TMVarDispNameVal
  | TMVarDispNameValDesc
  deriving (Show, Generic)

instance Serialise TMVarDisp

data TMVarRadix =
  TMVarBinary
  | TMVarOctal
  | TMVarDecimal
  | TMVarHex
  | TMVarNormal
  deriving (Show, Generic)

instance Serialise TMVarRadix



data TMVarParamDef = TMVarParamDef {
  _tmvpName :: !ShortText
  , _tmvpNat :: !TMVarParamModifier
  , _tmvpDisDesc :: !ShortText
  , _tmvpDisp :: !Bool
  , _tmvpJustify :: TMVarAlignment
  , _tmvpNewline :: !Bool
  , _tmvpDispCols :: !TMVarDisp
  , _tmvpRadix :: !TMVarRadix
  , _tmvpOffset :: !BitOffset
  } deriving (Show, Generic)

instance Serialise TMVarParamDef

-- | Specifies the parameters contained in the packet. Fixed packets vary only
-- when supercommutated. Variable packets can have groups, fixed repeaters and
-- choices.
data TMPacketParams =
  TMFixedParams (Vector TMParamLocation)
  | TMVariableParams {
    _tmvpTPSD :: !Int
    , _tmvpDfhSize :: !Word8
    , _tmvpParams :: Vector TMVarParamDef
  }
  deriving (Show, Generic)

instance Serialise TMPacketParams

-- | The TM packet definition. All information to extract the contents of a
-- packet is contained here.
data TMPacketDef = TMPacketDef {
    _tmpdSPID :: !SPID
    , _tmpdName :: !ShortText
    , _tmpdDescr :: !ShortText
    , _tmpdType :: !PUSType
    , _tmpdSubType :: !PUSSubType
    , _tmpdApid :: !APID
    , _tmpdPI1Val :: !Int
    , _tmpdPI2Val :: !Int
    , _tmpdUnit :: !ShortText
    , _tmpdTime :: !Bool
    , _tmpdInter :: Maybe (TimeSpn MilliSeconds)
    , _tmpdValid :: !Bool
    , _tmpdCheck :: !Bool
    , _tmpdEvent :: !PIDEvent
    , _tmpdParams :: TMPacketParams
    } deriving(Show, Generic)
makeLenses ''TMPacketDef


instance Serialise TMPacketDef

-- | The tuple (APID, PUSType, PUSSubType, PI1, PI2) which is the lookup key
-- for the packet definition
data TMPacketKey = TMPacketKey !APID !PUSType !PUSSubType !Int64 !Int64
  deriving (Eq, Show, Generic)

instance Hashable TMPacketKey
instance Serialise TMPacketKey

-- | The definition of the map which is used. Currently this is a immutable
-- hash table
type TMPacketMap = IHashTable TMPacketKey TMPacketDef


-- | Key into the packet identification criteria. Basically a tuple (APID, Type, Subtype).
data ApidKey =
  ApidKey !Word8
          !Word8
          !Word16
  deriving (Show, Generic)

instance Eq ApidKey where
  (ApidKey t st ap) == (ApidKey t2 st2 ap2) = t == t2 && st == st2 && ap == ap2

instance Ord ApidKey where
  compare (ApidKey t1 st1 ap1) (ApidKey t2 st2 ap2) = case compare ap1 ap2 of
    EQ -> case compare t1 t2 of
      EQ -> compare st1 st2
      x  -> x
    x -> x

instance Hashable ApidKey
instance Serialise ApidKey

-- | Create an ApidKey
mkApidKey :: APID -> PUSType -> PUSSubType -> ApidKey
mkApidKey (APID apid) t st =
  ApidKey (getPUSTypeVal t) (getPUSSubTypeVal st) apid

-- | Key into the packet identification criteria. Basically a tuple of (Type, Subtype).
data TypeSubTypeKey =
  TypeSubTypeKey !Word8
                 !Word8
  deriving (Show, Eq, Generic)

instance Ord TypeSubTypeKey where
  compare (TypeSubTypeKey t1 st1) (TypeSubTypeKey t2 st2) =
    case compare t1 t2 of
      EQ -> compare st1 st2
      x  -> x

instance Hashable TypeSubTypeKey
instance Serialise TypeSubTypeKey

-- | Create a 'TypeSubTypeKey'
mkTypeSubTypeKey :: PUSType -> PUSSubType -> TypeSubTypeKey
mkTypeSubTypeKey t st = TypeSubTypeKey (getPUSTypeVal t) (getPUSSubTypeVal st)


-- | A packet identification criteria. It is used to determine, where the PI1
-- and PI2 values are to be extracted.
data PacketIDCriteria = PacketIDCriteria {
  _pidcAPID :: Maybe APID
  , _pidcType :: PUSType
  , _pidcSubType :: PUSSubType
  , _pidcPIs :: TMPIDefs
  } deriving (Show, Generic)

instance Serialise PacketIDCriteria


-- | A search index. The lookup of packet identification criterias is a multiple
-- step process. First the tuple (APID, Type, Subtype) is looked up. If it is
-- found, it is used. If not, it is searched again with a (Type, Subtype) tuple.
-- If it is found, it is used. If not, it is specified that PI1 and PI2 values
-- should be put to 0
data PICSearchIndex = PICSearchIndex {
  _picSiApidMap :: IHashTable ApidKey PacketIDCriteria
  , _picSiMap :: IHashTable TypeSubTypeKey PacketIDCriteria
  } deriving (Show, Generic)

emptyPICSearchIndex :: PICSearchIndex
emptyPICSearchIndex = runST $ do
  PICSearchIndex
    <$> (HT.new >>= HT.unsafeFreeze)
    <*> (HT.new >>= HT.unsafeFreeze)

instance Serialise PICSearchIndex where
  encode = encodeSearchIndex
  decode = decodeSearchIndex

encodeSearchIndex :: PICSearchIndex -> Encoding
encodeSearchIndex idx =
  encodeListLen 2 <> encodeHashTable (_picSiApidMap idx) <> encodeHashTable
    (_picSiMap idx)


decodeSearchIndex :: Decoder s PICSearchIndex
decodeSearchIndex = do
  _len <- decodeListLen
  PICSearchIndex <$> decodeHashTable <*> decodeHashTable


picVecToApidMap
  :: Vector PacketIDCriteria -> IHashTable ApidKey PacketIDCriteria
picVecToApidMap pics = runST $ do
  ht <- HT.new
  V.mapM_ (ins ht) pics
  HT.unsafeFreeze ht
 where
  ins ht pic@PacketIDCriteria {..} = do
    case _pidcAPID of
      Just apid -> HT.insert ht (mkApidKey apid _pidcType _pidcSubType) pic
      _         -> return ()


picVecToTypeMap
  :: Vector PacketIDCriteria -> IHashTable TypeSubTypeKey PacketIDCriteria
picVecToTypeMap pics = runST $ do
  ht <- HT.new
  V.mapM_ (ins ht) pics
  HT.unsafeFreeze ht
 where
  ins ht pic@PacketIDCriteria {..} =
    HT.insert ht (mkTypeSubTypeKey _pidcType _pidcSubType) pic

-- | Create a search index from a Vector of 'PacketIDCriteria'
mkPICSearchIndex :: Vector PacketIDCriteria -> PICSearchIndex
mkPICSearchIndex pics =
  PICSearchIndex (picVecToApidMap pics) (picVecToTypeMap pics)


-- | This is one of the most important functions for packet identification.
-- It checks for the currently received packet, where the location for the
-- PI1 and PI2 values is. Returns either Just a 'TMPIDefs' specifying the
-- exact location to extract the PI1 and PI2 values.
-- If Nothing is returned, PI1 and PI2 should be set to 0 when searching the
-- correct 'TMPacketDef' entry to determine how to extract values
picFind :: PICSearchIndex -> APID -> PUSType -> PUSSubType -> Maybe TMPIDefs
picFind PICSearchIndex {..} apid typ subtype =
  case HT.ilookup _picSiApidMap (mkApidKey apid typ subtype) of
    Just val -> Just (_pidcPIs val)
    Nothing  -> _pidcPIs <$> HT.ilookup _picSiMap (mkTypeSubTypeKey typ subtype)
