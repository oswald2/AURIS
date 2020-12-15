{-# LANGUAGE
  AllowAmbiguousTypes
#-}
{-|
Module      : General.PUSTypes
Description : Collections of various PUS types
Copyright   : (c) Michael Oswald, 2019
License     : BSD-3
Maintainer  : michael.oswald@onikudaki.net
Stability   : experimental
Portability : POSIX

This module is a collection of various simple PUS types
-}
module General.PUSTypes
  ( VCID(..)
  , EduVCID(..)
  , mkVCID
  , vcidBuilder
  , vcidParser
  , SCID(..)
  , mkSCID
  , scidBuilder
  , scidParser
  , MAPID
  , mkMAPID
  , getMAPID
  , mapIDBuilder
  , mapIDParser
  , mapIDControl
  , Flag
  , toFlag
  , fromFlag
  , toBool
  , Ready(..)
  , Enable(..)
  , OnOff(..)
  , Initialized(..)
  , Good(..)
  , RequestID
  , getRqstID
  , mkRqstID
  , nextRqstID
  , saveRqstID
  , loadRqstID
  , TransmissionMode(..)
  , transmissionModeBuilder
  , transmissionModeParser
  , PUSType(..)
  , mkPUSType
  , PUSSubType(..)
  , mkPUSSubType
  , pusTypeBuilder
  , pusSubTypeBuilder
  , pusTypeParser
  , pusSubTypeParser
  , SSC
  , getSSC
  , mkSSC
  , nextSSC
  , SourceID(..)
  , mkSourceID
  , sourceIDBuilder
  , sourceIDParser
  , TMSegmentLen(..)
  , tmSegmentLength
  , SPID(..)
  , PTC(..)
  , PFC(..)
  )
where


import           RIO                     hiding ( Builder )
import qualified RIO.Text                      as T
import qualified RIO.ByteString.Lazy           as BL
import           Codec.Serialise
import           Data.Binary
import           Data.Aeson
import           ByteString.StrictBuilder
import           Data.Attoparsec.ByteString     ( Parser )
import qualified Data.Attoparsec.ByteString    as A
import qualified Data.Attoparsec.Binary        as A
import           Data.Bits
import           System.Directory
import           System.FilePath
import           Formatting


-- | Virtual Channel ID
newtype VCID = VCID { getVCID :: Word8 }
    deriving (Eq, Ord, Num, Show, Read, Generic)

-- | Smart constructor for the Virtual Channel ID
mkVCID :: Word8 -> VCID
mkVCID = VCID

instance NFData VCID
instance Binary VCID
instance Serialise VCID
instance FromJSON VCID
instance ToJSON VCID where
  toEncoding = genericToEncoding defaultOptions
instance Hashable VCID

instance Display VCID where
  display (VCID x) = display x

-- | A buidler for the VCID
vcidBuilder :: VCID -> Builder
vcidBuilder (VCID x) = word8 x

-- | A parser for the VCID
vcidParser :: Parser VCID
vcidParser = VCID <$> A.anyWord8


-- | The Spacecraft ID
newtype SCID = SCID { getSCID :: Word16 }
    deriving (Eq, Ord, Show, Read, Generic)

-- | Smart constructor for the S/C ID
mkSCID :: Word16 -> SCID
mkSCID = SCID

instance NFData SCID
instance Binary SCID
instance Serialise SCID
instance FromJSON SCID
instance ToJSON SCID where
  toEncoding = genericToEncoding defaultOptions

instance Display SCID where
  display (SCID s) = display s

-- | Builder for the S/C ID
scidBuilder :: SCID -> Builder
scidBuilder (SCID x) = word16BE x

-- | Parser for the S/C ID
scidParser :: Parser SCID
scidParser = SCID <$> A.anyWord16be



-- | The MAPID (Mulitplexer Access Point ID) used in the TC segmentation
-- layer
newtype MAPID = MAPID { getMAPID :: Word8 }
    deriving (Eq, Ord, Num, Show, Read, Generic)

-- | Smart constructor for the MAPID
mkMAPID :: Word8 -> MAPID
mkMAPID x = MAPID (x .&. 0x3F)

instance NFData MAPID
instance Binary MAPID
instance Serialise MAPID
instance FromJSON MAPID
instance ToJSON MAPID where
  toEncoding = genericToEncoding defaultOptions

-- | Builder for the MAPDI
mapIDBuilder :: MAPID -> Builder
mapIDBuilder (MAPID x) = word8 x

-- | Parser for the MAPID
mapIDParser :: Parser MAPID
mapIDParser = MAPID <$> A.anyWord8

-- | A special MAPID value which specifies, that this segment is not a
-- TC segment, but a control segment which commands the segmentation layer
-- on-board. Used in the TC authentication
mapIDControl :: MAPID
mapIDControl = MAPID 63



class FlagDisplay a where
  displayFlag :: Bool -> Text

-- | 'Flag' for Ready/Not Ready
data Ready = Ready
-- | 'Flag' for Enable/Disable
data Enable = Enable
-- | 'Flag' for On/Off
data OnOff = OnOff
-- | 'Flag' for Initial/Not Initial. Used in one-time initialisations
data Initialized = Initialized
-- | 'Flag' for quality
data Good = Good

instance FlagDisplay Ready where
  displayFlag True  = "READY"
  displayFlag False = "NOT READY"

instance FlagDisplay Enable where
  displayFlag True  = "ENABLED"
  displayFlag False = "DISABLED"

instance FlagDisplay OnOff where
  displayFlag True  = "ON"
  displayFlag False = "OFF"

instance FlagDisplay Initialized where
  displayFlag True  = "INIT"
  displayFlag False = "UNINIT"

instance FlagDisplay Good where
  displayFlag True  = "GOOD"
  displayFlag False = "BAD"


-- | Generic flag type. To be used with the types above (or new ones)
newtype Flag a = MkFlag Bool
    deriving (Eq, Ord, Show, Read, Generic)

instance Serialise (Flag a)
instance FromJSON (Flag a)
instance ToJSON (Flag a) where
  toEncoding = genericToEncoding defaultOptions


instance FlagDisplay a => Display (Flag a) where
  textDisplay (MkFlag x) = displayFlag @a x

-- | Converts a type with the given Bool to a 'Flag'
toFlag :: t -> Bool -> Flag t
toFlag _ = MkFlag

-- | Converts from the 'Flag' to a 'Bool'. Takes also the type itself
fromFlag :: t -> Flag t -> Bool
fromFlag _ (MkFlag b) = b

-- | Direct conversion from the 'Flag' to a bool, suitable in if expressions
toBool :: Flag t -> Bool
toBool (MkFlag b) = b

-- | The Request ID type
newtype RequestID = RequestID { getRqstID :: Word32 }
    deriving (Eq, Ord, Num, Show, Read, Generic)

-- | Smart constructor for the 'RequestID'
mkRqstID :: Word32 -> RequestID
mkRqstID = RequestID

nextRqstID :: RequestID -> RequestID
nextRqstID (RequestID x) = RequestID (x + 1)

instance NFData RequestID
instance Binary RequestID
instance Serialise RequestID
instance FromJSON RequestID
instance ToJSON RequestID where
  toEncoding = genericToEncoding defaultOptions

saveRqstID :: RequestID -> IO ()
saveRqstID rqstID = do
  home <- getHomeDirectory
  let path = home </> ".config/AURIS"
      file = path </> "RequestID.raw"
  createDirectoryIfMissing True path
  writeFileBinary file (BL.toStrict (serialise rqstID))

loadRqstID :: IO RequestID
loadRqstID = do
  home <- getHomeDirectory
  let path = home </> ".config/AURIS"
      file = path </> "RequestID.raw"
  exist <- doesFileExist file
  if exist
    then do
      res <- deserialiseOrFail . BL.fromStrict <$> readFileBinary file
      case res of
        Left _ -> return (mkRqstID 0)
        Right i   -> return i
    else return (mkRqstID 0)


-- | The Transmission Mode. Can be AD or BD
data TransmissionMode = AD | BD
    deriving (Eq, Ord, Enum, Show, Read, Generic)


instance NFData TransmissionMode
instance Binary TransmissionMode
instance Serialise TransmissionMode
instance FromJSON TransmissionMode
instance ToJSON TransmissionMode where
  toEncoding = genericToEncoding defaultOptions

-- | Builder for the 'TransmissionMode'
transmissionModeBuilder :: TransmissionMode -> Builder
transmissionModeBuilder AD = word8 0
transmissionModeBuilder BD = word8 1

-- | Parser for the 'TransmissionMode'
transmissionModeParser :: Parser TransmissionMode
transmissionModeParser = do
  val <- A.anyWord8
  case val of
    0 -> pure AD
    _ -> pure BD

-- | PUS Packet Type
newtype PUSType = PUSType { getPUSTypeVal :: Word8 }
    deriving (Eq, Ord, Num, Show, Read, Generic)

-- | Smart constructor for the 'PUSType'
mkPUSType :: Word8 -> PUSType
mkPUSType = PUSType


instance Display PUSType where
  textDisplay (PUSType x) = sformat int x


-- | PUS Sub Type
newtype PUSSubType = PUSSubType { getPUSSubTypeVal :: Word8 }
    deriving (Eq, Ord, Num, Show, Read, Generic)

-- | Smart constructor for the 'PUSSubType'
mkPUSSubType :: Word8 -> PUSSubType
mkPUSSubType = PUSSubType

instance Display PUSSubType where
  textDisplay (PUSSubType x) = sformat int x


-- | Builder for the 'PUSType'
pusTypeBuilder :: PUSType -> Builder
pusTypeBuilder (PUSType x) = word8 x

-- | Builder for the 'PUSSubType'
pusSubTypeBuilder :: PUSSubType -> Builder
pusSubTypeBuilder (PUSSubType x) = word8 x

-- | Parser for the 'PUSType'
pusTypeParser :: Parser PUSType
pusTypeParser = PUSType <$> A.anyWord8

-- | Parser for the 'PUSSubType'
pusSubTypeParser :: Parser PUSSubType
pusSubTypeParser = PUSSubType <$> A.anyWord8

instance NFData PUSType
instance Hashable PUSType
instance Serialise PUSType
instance FromJSON PUSType
instance ToJSON PUSType where
  toEncoding = genericToEncoding defaultOptions


instance NFData PUSSubType
instance Hashable PUSSubType
instance Serialise PUSSubType
instance FromJSON PUSSubType
instance ToJSON PUSSubType where
  toEncoding = genericToEncoding defaultOptions


-- | Type for the source sequence count
newtype SSC = SSC { getSSC :: Word16 }
    deriving (Eq, Ord, Enum, Num, Real, Integral, Show, Read, Generic)

-- | Smart constructor for a 'SSC'
mkSSC :: Word16 -> SSC
mkSSC = SSC

-- | Gives the next 'SSC' in order
nextSSC :: SSC -> SSC
nextSSC (SSC x) = SSC (x + 1)

instance NFData SSC
instance Binary SSC
instance Serialise SSC
instance FromJSON SSC
instance ToJSON SSC where
  toEncoding = genericToEncoding defaultOptions
instance Hashable SSC

instance Display SSC where
  display (SSC x) = display x

newtype SourceID = SourceID { getSourceID :: Word8 }
    deriving (Eq, Ord, Num, Show, Read, Generic)

mkSourceID :: Word8 -> SourceID
mkSourceID = SourceID

instance Binary SourceID
instance Serialise SourceID
instance FromJSON SourceID
instance ToJSON SourceID where
  toEncoding = genericToEncoding defaultOptions
instance NFData SourceID


-- | A buidler for the VCID
sourceIDBuilder :: SourceID -> Builder
sourceIDBuilder (SourceID x) = word8 x

-- | A parser for the VCID
sourceIDParser :: Parser SourceID
sourceIDParser = SourceID <$> A.anyWord8



-- | Used for specifying the segment length for TM frames
data TMSegmentLen = TMSegment256
    | TMSegment512
    | TMSegment1024
    | TMSegment65536
      deriving (Show, Read, Eq, Ord, Enum, Generic)

instance Binary TMSegmentLen
instance Serialise TMSegmentLen
instance FromJSON TMSegmentLen
instance ToJSON TMSegmentLen where
  toEncoding = genericToEncoding defaultOptions


-- | returns the length of the segment in bytes
tmSegmentLength :: TMSegmentLen -> Int
tmSegmentLength TMSegment256   = 256
tmSegmentLength TMSegment512   = 512
tmSegmentLength TMSegment1024  = 1024
tmSegmentLength TMSegment65536 = 65536


-- | The SPID (SCOS Packet ID)
newtype SPID = SPID Word32
    deriving (Eq, Ord, Show, Read, Generic)

instance Hashable SPID
instance Serialise SPID
instance FromJSON SPID
instance ToJSON SPID where
  toEncoding = genericToEncoding defaultOptions

instance Display SPID where
  display (SPID x) = display x

newtype PTC = PTC Int
    deriving (Eq, Ord, Num, Show, Read, Generic)

instance Serialise PTC
instance FromJSON PTC
instance ToJSON PTC where
  toEncoding = genericToEncoding defaultOptions

instance Display PTC where
  textDisplay = T.pack . show


newtype PFC = PFC Int
    deriving (Eq, Ord, Num, Show, Read, Generic)

instance Serialise PFC
instance FromJSON PFC
instance ToJSON PFC where
  toEncoding = genericToEncoding defaultOptions

instance Display PFC where
  textDisplay = T.pack . show


data EduVCID = 
  IsVCID !VCID
  | IsSCOE 
  deriving (Eq, Show, Generic)


instance Serialise EduVCID
instance FromJSON EduVCID
instance ToJSON EduVCID where
  toEncoding = genericToEncoding defaultOptions

instance Display EduVCID where
  display (IsVCID vcid) = display vcid
  display IsSCOE = display @Text "SCOE"
