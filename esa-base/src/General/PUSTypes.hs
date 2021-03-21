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
    , getVCID
    , vcidBuilder
    , vcidParser
    , SCID(..)
    , mkSCID
    , getSCID
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
    , getPUSTypeVal
    , PUSSubType(..)
    , mkPUSSubType
    , getPUSSubTypeVal
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
    , getSourceID
    , sourceIDBuilder
    , sourceIDParser
    , TMSegmentLen(..)
    , tmSegmentLength
    , SPID(..)
    , PTC(..)
    , PFC(..)
    , PUSPacketType(..)
    , PktID(..)
    , pktIdDisplayPretty
    , pktIdVersion
    , pktIdType
    , pktIdSetType
    , pktIdDfh
    , pktIdAPID
    , SeqControl(..)
    , ProtocolLevel(..)
    , CommandType(..)
    , DirectiveProtocolLevel(..)
    , Destination(..)
    , DirectiveDestination(..)
    , ScoeDestination(..)
    ) where


import           RIO                     hiding ( Builder )
import qualified RIO.ByteString.Lazy           as BL
import           Codec.Serialise                ( Serialise
                                                , deserialiseOrFail
                                                , serialise
                                                )
import           Data.Aeson                     ( defaultOptions
                                                , genericToEncoding
                                                , FromJSON
                                                , ToJSON(toEncoding)
                                                )
import           ByteString.StrictBuilder       ( Builder
                                                , word16BE
                                                , word8
                                                )
import           Data.Attoparsec.ByteString     ( Parser )
import qualified Data.Attoparsec.ByteString    as A
import qualified Data.Attoparsec.Binary        as A
import           Data.Bits                      ( Bits((.&.), shiftR, (.|.)) )
import           System.Directory               ( createDirectoryIfMissing
                                                , doesFileExist
                                                , getHomeDirectory
                                                )
import           System.FilePath                ( (</>) )
import           General.APID                   ( APID(APID) )

import           Protocol.ProtocolInterfaces

-- | Virtual Channel ID
newtype VCID = VCID Word8
    deriving (Eq, Ord, Num, Show, Read, Generic)


getVCID :: VCID -> Word8
getVCID (VCID x) = x

-- | Smart constructor for the Virtual Channel ID
mkVCID :: Word8 -> VCID
mkVCID = VCID

instance NFData VCID
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
newtype SCID = SCID Word16
    deriving (Eq, Ord, Show, Read, Generic)

getSCID :: SCID -> Word16
getSCID (SCID x) = x

-- | Smart constructor for the S/C ID
mkSCID :: Word16 -> SCID
mkSCID = SCID

instance NFData SCID
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
newtype MAPID = MAPID Word8
    deriving (Eq, Ord, Num, Show, Read, Generic)

getMAPID :: MAPID -> Word8
getMAPID (MAPID x) = x

-- | Smart constructor for the MAPID
mkMAPID :: Word8 -> MAPID
mkMAPID x = MAPID (x .&. 0x3F)

instance NFData MAPID
instance Serialise MAPID
instance FromJSON MAPID
instance ToJSON MAPID where
    toEncoding = genericToEncoding defaultOptions

instance Display MAPID where
    display (MAPID x) = display x

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
    deriving Generic
-- | 'Flag' for Enable/Disable
data Enable = Enable
    deriving Generic
-- | 'Flag' for On/Off
data OnOff = OnOff
    deriving Generic
-- | 'Flag' for Initial/Not Initial. Used in one-time initialisations
data Initialized = Initialized
    deriving Generic
-- | 'Flag' for quality
data Good = Good
    deriving Generic

instance NFData Good
instance NFData Ready
instance NFData Enable
instance NFData OnOff
instance NFData Initialized


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

instance NFData a => NFData (Flag a)
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
newtype RequestID = RequestID Word32
    deriving (Eq, Ord, Num, Show, Read, Hashable, Generic)

getRqstID :: RequestID -> Word32
getRqstID (RequestID x) = x

-- | Smart constructor for the 'RequestID'
mkRqstID :: Word32 -> RequestID
mkRqstID = RequestID

nextRqstID :: RequestID -> RequestID
nextRqstID (RequestID x) = RequestID (x + 1)

instance NFData RequestID
instance Serialise RequestID
instance FromJSON RequestID
instance ToJSON RequestID where
    toEncoding = genericToEncoding defaultOptions


instance Display RequestID where
    display (RequestID x) = display x

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
                Left  _ -> return (mkRqstID 0)
                Right i -> return i
        else return (mkRqstID 0)


-- | The Transmission Mode. Can be AD or BD
data TransmissionMode = AD | BD
    deriving (Eq, Ord, Enum, Show, Read, Generic)


instance NFData TransmissionMode
instance Serialise TransmissionMode
instance FromJSON TransmissionMode
instance ToJSON TransmissionMode where
    toEncoding = genericToEncoding defaultOptions

instance Display TransmissionMode where
    display AD = "AD"
    display BD = "BD"


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
newtype PUSType = PUSType Word8
    deriving (Eq, Ord, Num, Show, Read, Generic)


getPUSTypeVal :: PUSType -> Word8
getPUSTypeVal (PUSType x) = x

-- | Smart constructor for the 'PUSType'
mkPUSType :: Word8 -> PUSType
mkPUSType = PUSType


instance Display PUSType where
    display (PUSType x) = display x


-- | PUS Sub Type
newtype PUSSubType = PUSSubType Word8
    deriving (Eq, Ord, Num, Show, Read, Generic)

getPUSSubTypeVal :: PUSSubType -> Word8
getPUSSubTypeVal (PUSSubType x) = x


-- | Smart constructor for the 'PUSSubType'
mkPUSSubType :: Word8 -> PUSSubType
mkPUSSubType = PUSSubType

instance Display PUSSubType where
    display (PUSSubType x) = display x


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



data PUSPacketType = PUSTM | PUSTC deriving (Ord, Eq, Enum, Show, Read, Generic)

instance Serialise PUSPacketType
instance FromJSON PUSPacketType
instance ToJSON PUSPacketType where
    toEncoding = genericToEncoding defaultOptions

instance Display PUSPacketType where
    display PUSTM = "TM"
    display PUSTC = "TC"



newtype PktID = PktID Word16
  deriving (Eq, Ord, Enum, Num, Real, Integral, Show, Read, Generic)

instance NFData PktID
instance Hashable PktID
instance Serialise PktID
instance FromJSON PktID
instance ToJSON PktID where
    toEncoding = genericToEncoding defaultOptions

instance Display PktID where
    display (PktID x) = display x

pktIdDisplayPretty :: PktID -> Utf8Builder
pktIdDisplayPretty x =
    "Version: "
        <> display (pktIdVersion x)
        <> " Type: "
        <> display (pktIdType x)
        <> " DFH: "
        <> if pktIdDfh x
               then "True"
               else "False" <> " APID: " <> display (pktIdAPID x)

pktIdVersion :: PktID -> Word8
pktIdVersion (PktID x) =
    fromIntegral $ (x .&. 0b1110_0000_0000_0000) `shiftR` 13

pktIdType :: PktID -> PUSPacketType
pktIdType (PktID x) =
    if (x .&. 0b0001_0000_0000_0000) /= 0 then PUSTC else PUSTM

pktIdSetType :: PktID -> PUSPacketType -> PktID
pktIdSetType (PktID x) PUSTM = PktID (x .&. 0b1110_1111_1111_1111)
pktIdSetType (PktID x) PUSTC = PktID (x .|. 0b0001_0000_0000_0000)

pktIdDfh :: PktID -> Bool
pktIdDfh (PktID x) = (x .&. 0b0000_1000_0000_0000) /= 0

pktIdAPID :: PktID -> APID
pktIdAPID (PktID x) = APID (x .&. 0x7FF)

newtype SeqControl = SeqControl Word16
  deriving (Eq, Ord, Enum, Num, Real, Integral, Show, Read, Generic)

instance NFData SeqControl
instance Hashable SeqControl
instance Serialise SeqControl
instance FromJSON SeqControl
instance ToJSON SeqControl where
    toEncoding = genericToEncoding defaultOptions

instance Display SeqControl where
    display (SeqControl x) = display x


-- | Type for the source sequence count
newtype SSC = SSC Word16
    deriving (Eq, Ord, Enum, Num, Real, Integral, Show, Read, Generic)


getSSC :: SSC -> Word16
getSSC (SSC x) = x

-- | Smart constructor for a 'SSC'
mkSSC :: Word16 -> SSC
mkSSC = SSC

-- | Gives the next 'SSC' in order
nextSSC :: SSC -> SSC
nextSSC (SSC x) = SSC (x + 1)

instance NFData SSC
instance Serialise SSC
instance FromJSON SSC
instance ToJSON SSC where
    toEncoding = genericToEncoding defaultOptions
instance Hashable SSC

instance Display SSC where
    display (SSC x) = display x

newtype SourceID = SourceID Word8
    deriving (Eq, Ord, Num, Show, Read, Generic)

getSourceID :: SourceID -> Word8
getSourceID (SourceID x) = x

mkSourceID :: Word8 -> SourceID
mkSourceID = SourceID

instance Serialise SourceID
instance FromJSON SourceID
instance ToJSON SourceID where
    toEncoding = genericToEncoding defaultOptions
instance NFData SourceID

instance Display SourceID where
    display (SourceID x) = display x

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

instance NFData TMSegmentLen
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
    display (PTC x) = display x


newtype PFC = PFC Int
    deriving (Eq, Ord, Num, Show, Read, Generic)

instance Serialise PFC
instance FromJSON PFC
instance ToJSON PFC where
    toEncoding = genericToEncoding defaultOptions

instance Display PFC where
    display (PFC x) = display x


data EduVCID =
  IsVCID !VCID
  | IsSCOE
  deriving (Eq, Show, Generic)

instance NFData EduVCID
instance Serialise EduVCID
instance FromJSON EduVCID
instance ToJSON EduVCID where
    toEncoding = genericToEncoding defaultOptions

instance Display EduVCID where
    display (IsVCID vcid) = display vcid
    display IsSCOE        = display @Text "SCOE"


-- | Determines if the 'TCRequest' is a spacecraft command or a command destined 
-- for a SCOE. Space commands are encoded slightly different in most protocols.
-- Also, the 'ProtocolLevel' is given here to specify the encoding of the command
-- to be sent out.
data CommandType =
  Space ProtocolLevel
  | SCOE
    deriving (Eq, Ord, Show, Read, Generic)

instance NFData CommandType
instance Serialise CommandType
instance FromJSON CommandType
instance ToJSON CommandType where
    toEncoding = genericToEncoding defaultOptions

instance Display CommandType where
    display (Space level) = "SPACE (" <> display level <> ")"
    display SCOE          = "SCOE"


-- | The level on which the TC request should be encoded and sent. For various 
-- purposes, different levels are used. For assembly integration & testing mostly
-- packet based level is used, but also for some automated commanding when the AD 
-- mode handling is done outside of the system (e.g. Proba-3)
--
-- TC Frame level is when the AD mode is handled, but the encoding for transmission 
-- is done externally (normally CLTU). This is done e.g. in Galileo where TC Frames
-- are sent out to the encryption unit which encrypts and CLTU encodes the frames
-- before sending them out
--
-- For TCs going to spacecrafts this is normally done via CLTUs, hence this is the 
-- last level (lowest encoding level).
data ProtocolLevel =
  ProtLevelPacket
  | ProtLevelFrame
  | ProtLevelCltu
    deriving (Eq, Ord, Enum, Show, Read, Generic)

instance NFData ProtocolLevel
instance Serialise ProtocolLevel
instance FromJSON ProtocolLevel
instance ToJSON ProtocolLevel where
    toEncoding = genericToEncoding defaultOptions

instance Display ProtocolLevel where
    display ProtLevelPacket = "PACKET"
    display ProtLevelFrame  = "FRAME"
    display ProtLevelCltu   = "CLTU"


data DirectiveProtocolLevel =
  DirProtLevelFrame
  | DirProtLevelCltu
    deriving (Eq, Ord, Enum, Show, Read, Generic)

instance NFData DirectiveProtocolLevel
instance Serialise DirectiveProtocolLevel
instance FromJSON DirectiveProtocolLevel
instance ToJSON DirectiveProtocolLevel where
    toEncoding = genericToEncoding defaultOptions

instance Display DirectiveProtocolLevel where
    display DirProtLevelFrame = "FRAME"
    display DirProtLevelCltu  = "CLTU"




-- | Specifies the destination, where the command should be sent to. This can be 
-- the given interfaces (currently NCTRS, C&C and EDEN). For EDEN also the type 
-- (and protocol level) can be specified as EDEN is quite flexible and can handle 
-- full spacecraft as well as SCOE management.
data Destination =
  DestNctrs ProtocolInterface
  | DestCnc ProtocolInterface
  | DestEden ProtocolInterface CommandType
  deriving (Eq, Show, Read, Generic)

instance NFData Destination
instance Serialise Destination
instance FromJSON Destination
instance ToJSON Destination where
    toEncoding = genericToEncoding defaultOptions

instance Display Destination where
    display (DestNctrs i ) = display i
    display (DestCnc   i ) = display i
    display (DestEden i t) = display i <> " (" <> display t <> ")"


-- | The destination a 'TCDirective' can have. Since directives only make sense 
-- in spacecraft links and not SCOE links, the destination is different from the 
-- 'TCRequest' destination ('Destination'). 
data DirectiveDestination =
  DirDestNctrs ProtocolInterface
  | DirDestEden ProtocolInterface DirectiveProtocolLevel
  deriving (Eq, Show, Read, Generic)

instance NFData DirectiveDestination
instance Serialise DirectiveDestination
instance FromJSON DirectiveDestination
instance ToJSON DirectiveDestination where
    toEncoding = genericToEncoding defaultOptions

instance Display DirectiveDestination where
    display (DirDestNctrs i ) = display i
    display (DirDestEden i l) = display i <> " (" <> display l <> ")"


-- | Destination for SCOE commands. SCOE commands are special CCSDS packets which 
-- don't have a binary content, but are more text oriented. These commands can only 
-- be sent on the C&C protocol links as well as on the EDEN link, when the packet 
-- is contained in an 'EdenMessage'
data ScoeDestination =
  ScoeDestCnc ProtocolInterface
  | ScoeDestEden ProtocolInterface
  deriving (Eq, Show, Read, Generic)

instance NFData ScoeDestination
instance Serialise ScoeDestination
instance FromJSON ScoeDestination
instance ToJSON ScoeDestination where
    toEncoding = genericToEncoding defaultOptions

instance Display ScoeDestination where
    display (ScoeDestCnc  i) = display i
    display (ScoeDestEden i) = display i


instance ProtocolDestination Destination where
    destination (DestNctrs x ) = x
    destination (DestCnc   x ) = x
    destination (DestEden x _) = x

instance ProtocolDestination DirectiveDestination where
    destination (DirDestNctrs x ) = x
    destination (DirDestEden x _) = x

instance ProtocolDestination ScoeDestination where
    destination (ScoeDestCnc  x) = x
    destination (ScoeDestEden x) = x
