{-|
Module      : Data.PUS.Config
Description : Configuration for the PUS functionality
Copyright   : (c) Michael Oswald, 2019
License     : BSD-3
Maintainer  : michael.oswald@onikudaki.net
Stability   : experimental
Portability : POSIX

This module contains the data structure for configuration. The configuration can currently be read and written from files in
JSON (via the aeson library)
-}
module Data.PUS.Config
    (
    -- | The config data type itself
      Config(..)
    -- | Type for the CLTU code block size. Restricted to be 5 .. 8
    , CltuBlockSize(..)
    , NctrsConfig(..)
    , CncConfig(..)
    , EDENConfig(..)
    , SLEConfig(..)
    , SLEInstance(..)
    , SLEInstanceConfig(..)
    , SLERafConfig(..)
    , SLECltuConfig(..)
    , SlePlop(..)
    , SLEDeliveryMode(..)
    , SLEVersion(..)
    , sleInstanceCfgSII
    , NDIULiteConfig(..)
    , NdiuDirection(..)
    , VerificationConfig(..)
    , CLTUTrailer(..)
    , cltuBlockSizeAsWord8
    , defaultConfig
    , defaultNdiuConfig
    -- , writeConfigString
    , writeConfigJSON
    -- , loadConfigString
    , loadConfigJSON
    , getInterfaces
    , getDefaultInterface
    , getDefaultInterfaceName
    , getInterfaceMap
    ) where


import           Data.Aeson
import           Data.ByteString.Lazy          as B
import qualified Data.Text                     as T
import           Data.Text.Short                ( ShortText )
import qualified Data.Text.Short               as ST
import           RIO
import qualified RIO.HashMap                   as HM
import           RIO.List                       ( headMaybe )

import           General.PUSTypes

import           General.Time

import           Protocol.ProtocolInterfaces

import           Data.PUS.EncTime               ( CucEncoding(..) )
import           Data.PUS.MissionSpecific.Missions
import           Data.PUS.PUSPacket             ( HasCRC(..) )
import           Data.PUS.TMFrame               ( TMFrameConfig
                                                , defaultTMFrameConfig
                                                )

data NctrsConfig = NctrsConfig
    { cfgNctrsID      :: !Word16
    , cfgNctrsName    :: !Text
    , cfgNctrsHost    :: !Text
    , cfgNctrsPortTC  :: !Word16
    , cfgNctrsPortTM  :: !Word16
    , cfgNctrsPortADM :: !Word16
    }
    deriving (Eq, Generic)

instance FromJSON NctrsConfig
instance ToJSON NctrsConfig where
    toEncoding = genericToEncoding defaultOptions


data CncConfig = CncConfig
    {
  -- | A numerical ID, which needs to be unique for this C&C connection
      cfgCncID      :: !Word16
  -- | A name for the connection to be displayed in the GUI
    , cfgCncName    :: !Text
  -- | The host where to connect to 
    , cfgCncHost    :: !Text
  -- | The TM port to connect to 
    , cfgCncPortTM  :: !Word16
  -- | The TC port to connect to 
    , cfgCncPortTC  :: !Word16
    -- | Configures, if packets on the C&C protocol link should 
    -- be CRC checked or not 
    , cfgCncHasCRC  :: !HasCRC
    -- | Configures the format of the time for this C&C link
    , cfgCncCucTime :: !(Maybe CucEncoding)
    }
    deriving (Eq, Generic)

instance FromJSON CncConfig
instance ToJSON CncConfig where
    toEncoding = genericToEncoding defaultOptions


-- | Configuration for an EDEN connection. 
data EDENConfig = EDENConfig
    {
    -- | A numerical ID, which needs to be unique for this C&C connection
      cfgEdenID   :: !Word16
  -- | A name for the connection to be displayed in the GUI
    , cfgEdenName :: !Text
    -- | The host where to connect to 
    , cfgEdenHost :: !Text
    -- | The port where to connect to 
    , cfgEdenPort :: !Word16
    }
    deriving (Eq, Generic)

instance FromJSON EDENConfig
instance ToJSON EDENConfig where
    toEncoding = genericToEncoding defaultOptions

data SLEInstanceConfig =
  SLEInstRAF SLERafConfig
  | SLEInstRCF
  | SLEInstFCLTU SLECltuConfig
  deriving (Eq, Generic)

sleInstanceCfgSII :: SLEInstanceConfig -> Text
sleInstanceCfgSII (SLEInstRAF   raf ) = cfgSleRafSII raf
sleInstanceCfgSII (SLEInstFCLTU cltu) = cfgSleCltuSII cltu
sleInstanceCfgSII _                   = "undefined"


instance FromJSON SLEInstanceConfig
instance ToJSON SLEInstanceConfig where
    toEncoding = genericToEncoding defaultOptions


data SLEInstance = SLEInstance
    { cfgSleInstanceNr     :: !Word8
    , cfgSleInstanceConfig :: SLEInstanceConfig
    }
    deriving (Eq, Generic)

instance FromJSON SLEInstance
instance ToJSON SLEInstance where
    toEncoding = genericToEncoding defaultOptions


data SLEDeliveryMode =
  SLEOnlineComplete
  | SLEOnlineTimely
  | SLEOffline
  deriving (Eq, Generic)

instance FromJSON SLEDeliveryMode
instance ToJSON SLEDeliveryMode where
    toEncoding = genericToEncoding defaultOptions


data SLEVersion =
    SLEVersion1
    | SLEVersion2
    | SLEVersion3
    | SLEVersion4
    deriving (Eq, Ord, Show, Generic)

instance FromJSON SLEVersion
instance ToJSON SLEVersion where
    toEncoding = genericToEncoding defaultOptions

instance Display SLEVersion where
    display SLEVersion1 = "1"
    display SLEVersion2 = "2"
    display SLEVersion3 = "3"
    display SLEVersion4 = "4"

data SLERafConfig = SLERafConfig
    {
    -- | the SLE service instance ID for the RAF service 
      cfgSleRafSII     :: !Text
    , cfgSleRafVersion :: !SLEVersion
    , cfgSleRafPeerID  :: !Text
    , cfgSleRafPort    :: !Text
    }
    deriving (Eq, Generic)

instance FromJSON SLERafConfig
instance ToJSON SLERafConfig where
    toEncoding = genericToEncoding defaultOptions


data SlePlop =
  SlePLOP1
  | SlePLOP2
  deriving (Eq, Ord, Enum, Generic)

instance FromJSON SlePlop
instance ToJSON SlePlop where
    toEncoding = genericToEncoding defaultOptions


data SLECltuConfig = SLECltuConfig
    { cfgSleCltuSII     :: !Text
    , cfgSleCltuVersion :: !SLEVersion
    , cfgSleCltuPeerID  :: !Text
    , cfgSleCltuPort    :: !Text
    }
    deriving (Eq, Generic)

instance FromJSON SLECltuConfig
instance ToJSON SLECltuConfig where
    toEncoding = genericToEncoding defaultOptions



data SLEConfig = SLEConfig
    {
    -- | Path to the SLE config file for the Service Element 
      cfgSleSeConfig    :: !FilePath
    -- | Path to the SLE config file for the Proxy
    , cfgSleProxyConfig :: !FilePath
    -- | The peer ID of AURIS itself
    , cfgSlePeerID      :: !Text
    -- | A list of instance configurations
    , cfgSleInstances   :: [SLEInstance]
    }
    deriving (Eq, Generic)

instance FromJSON SLEConfig
instance ToJSON SLEConfig where
    toEncoding = genericToEncoding defaultOptions

data NdiuDirection = DirectionIn | DirectionOut | DirectionIO
    deriving (Eq, Ord, Enum, Show, Read, Generic)

instance FromJSON NdiuDirection
instance ToJSON NdiuDirection where
    toEncoding = genericToEncoding defaultOptions


data NDIULiteConfig = NDIULiteConfig
    { cfgNdiuHost              :: !Text
    , cfgNdiuPort              :: !Word16
    , cfgNdiuDirection         :: !NdiuDirection
    , cfgNdiuHeartbeatEnable   :: !Bool
    , cfgNdiuHeartbeatSendTime :: !Word32
    , cfgNdiuHeartbeatTimeout  :: !Word32
    , cfgNdiuID                :: !Word16
    , cfgNdiuName              :: !Text
    , cfgNdiuTMConfig          :: !TMFrameConfig
    }
    deriving (Eq, Show, Read, Generic)

instance FromJSON NDIULiteConfig
instance ToJSON NDIULiteConfig where
    toEncoding = genericToEncoding defaultOptions



-- | Configuration data for the TC verifications. All timeouts 
-- are specified in seconds
data VerificationConfig = VerificationConfig
    {
  -- | Specifies the timeout for the G and T stage. Both are set 
  -- together with he same timeout (ground reception and transmission)
      cfgTimeoutGT :: !Word16
  -- | Specifies the timeout for the O (on-board arrival) stage.
    , cfgTimeoutO  :: !Word16
  -- | Specifies the timeout for the A (acceptance) stage.
    , cfgTimeoutA  :: !Word16
  -- | Specifies the timeout for the S (start execution) stage.
    , cfgTimeoutS  :: !Word16
  -- | Specifies the timeout for the C (execution complete) stage.
    , cfgTimeoutC  :: !Word16
    }
    deriving (Eq, Generic)

instance FromJSON VerificationConfig
instance ToJSON VerificationConfig where
    toEncoding = genericToEncoding defaultOptions

defaultVerifConfig :: VerificationConfig
defaultVerifConfig = VerificationConfig { cfgTimeoutGT = 20
                                        , cfgTimeoutO  = 20
                                        , cfgTimeoutA  = 30
                                        , cfgTimeoutS  = 30
                                        , cfgTimeoutC  = 30
                                        }


data CLTUTrailer = Trailer0x55 | Trailer0xC5
    deriving (Eq, Ord, Enum, Show, Read, Generic)

instance FromJSON CLTUTrailer
instance ToJSON CLTUTrailer where
    toEncoding = genericToEncoding defaultOptions



-- | The configuration of the PUS functionality
data Config = Config
    {
    -- | The mission to be used. Determines the feature set
      cfgMission              :: !Mission
    -- | The block size that is used to encode/decode the CLTU
    , cfgCltuBlockSize        :: !CltuBlockSize
    -- | The type of the CLTU trailer sequence. Legacy is 0x55, while newer versions us 0xC5
    , cfgCLTUTrailer          :: !CLTUTrailer
    -- | If the socket interface is used, specifies Just portnumber, else Nothing
    , cfgInterfacePort        :: Maybe Word16
    -- | If the TC randomization is enabled by default
    , cfgRandomizerEnabled    :: !Bool
    -- | The start value of the randomizer used for standard ESA TC randomization
    , cfgRandomizerStartValue :: !Word8
    -- | The spacecraft ID used
    , cfgSCID                 :: SCID
    -- | A list of available virtual channels.
    , cfgVCIDs                :: [VCID]
    -- | Specifies the time epoch to be used for time handling
    , cfgEpoch                :: EpochType
    -- | Specified the used leap seconds
    , cfgLeapSeconds          :: LeapSeconds
    -- | Packets, which cannot be identified will be given this 
    -- SPID
    , cfgUnknownSPID          :: SPID
    -- | Specifies the configurations for the available NCTRS connections
    , cfgTMFrame              :: TMFrameConfig
    , cfgNCTRS                :: [NctrsConfig]
    -- | Specifies the configurations of the available C&C connections
    , cfgCnC                  :: [CncConfig]
    -- | Specifies the configuration of the available EDEN connections
    , cfgEDEN                 :: [EDENConfig]
    -- | Specifies the SLE interface configuration
    , cfgSLE                  :: Maybe SLEConfig
    -- | Specifies the configurations for NDIU connections
    , cfgNDIU                 :: [NDIULiteConfig]
    -- | Specifies default values for TC verifications
    , cfgVerification         :: VerificationConfig
    -- | Determines, whether incoming TM Frames are stored in the DB. This
    -- can be useful for replays of TM
    , cfgStoreTMFrames        :: !Bool
    -- | Determines, whether incoming raw PUS Packst are stored in the DB.
    -- This can be usefull for replays of TM when no TM Frames are available
    -- (e.g. C&C or EDEN connections)
    , cfgStorePUSPackets      :: !Bool
    }
    deriving (Eq, Generic)


instance FromJSON Config
instance ToJSON Config where
    toEncoding = genericToEncoding defaultOptions

-- | Specifies the CLTU block size. Since there are only very few
-- values allowed (5,6,7,8), we do an enumeration
data CltuBlockSize =
    CltuBS_5
    | CltuBS_6
    | CltuBS_7
    | CltuBS_8
    deriving (Eq, Enum, Show, Read, Generic)

instance FromJSON CltuBlockSize
instance ToJSON CltuBlockSize where
    toEncoding = genericToEncoding defaultOptions

cltuBlockSizeAsWord8 :: CltuBlockSize -> Word8
cltuBlockSizeAsWord8 CltuBS_5 = 5
cltuBlockSizeAsWord8 CltuBS_6 = 6
cltuBlockSizeAsWord8 CltuBS_7 = 7
cltuBlockSizeAsWord8 CltuBS_8 = 8


defaultEdenConfig :: EDENConfig
defaultEdenConfig = EDENConfig { cfgEdenID   = 1
                               , cfgEdenName = "EDEN 1"
                               , cfgEdenHost = "localhost"
                               , cfgEdenPort = 40300
                               }


defaultNctrsConfig :: NctrsConfig
defaultNctrsConfig = NctrsConfig { cfgNctrsID      = 1
                                 , cfgNctrsName    = "NCTRS A"
                                 , cfgNctrsHost    = "localhost"
                                 , cfgNctrsPortTC  = 20009
                                 , cfgNctrsPortTM  = 2502
                                 , cfgNctrsPortADM = 20010
                                 }

defaultCncConfig :: CncConfig
defaultCncConfig = CncConfig { cfgCncID      = 1
                             , cfgCncName    = "SCOE 1"
                             , cfgCncHost    = "localhost"
                             , cfgCncPortTM  = 10000
                             , cfgCncPortTC  = 11000
                             , cfgCncHasCRC  = HasCRC
                             , cfgCncCucTime = Nothing
                             }


defaultSleConfig :: SLEConfig
defaultSleConfig = SLEConfig
    { cfgSleSeConfig    = "sle/SE_PROV_Config.txt"
    , cfgSleProxyConfig = "sle/PROXY_PROV_Config.txt"
    , cfgSlePeerID      = "AURIS"
    , cfgSleInstances   =
        [ SLEInstance
            { cfgSleInstanceNr     = 1
            , cfgSleInstanceConfig = SLEInstRAF SLERafConfig
                { cfgSleRafSII     =
                    "sagr=3.spack=facility-PASS1.rsl-fg=1.raf=onlc1"
                , cfgSleRafVersion = SLEVersion3
                , cfgSleRafPeerID  = "PARAGONTT"
                , cfgSleRafPort    = "PORT_TM1"
                }
            }
        , SLEInstance
            { cfgSleInstanceNr     = 1
            , cfgSleInstanceConfig = SLEInstFCLTU SLECltuConfig
                { cfgSleCltuSII     =
                    "sagr=3.spack=facility-PASS1.rsl-fg=1.cltu=cltu1"
                , cfgSleCltuVersion = SLEVersion3
                , cfgSleCltuPeerID  = "PARAGONTT"
                , cfgSleCltuPort    = "PORT_TC1"
                }
            }
        ]
    }


-- | default configuration for NDIULite protocol
defaultNdiuConfig :: NDIULiteConfig
defaultNdiuConfig = NDIULiteConfig { cfgNdiuHost              = "localhost"
                                   , cfgNdiuPort              = 5005
                                   , cfgNdiuDirection         = DirectionOut
                                   , cfgNdiuHeartbeatEnable   = True
                                   , cfgNdiuHeartbeatSendTime = 5
                                   , cfgNdiuHeartbeatTimeout  = 10
                                   , cfgNdiuID                = 1
                                   , cfgNdiuName              = "NDIU"
                                   , cfgNdiuTMConfig = defaultTMFrameConfig
                                   }



-- | a default configuration with typical values.
defaultConfig :: Config
defaultConfig = Config { cfgMission              = MissionDefault
                       , cfgCltuBlockSize        = CltuBS_8
                       , cfgCLTUTrailer          = Trailer0xC5
                       , cfgInterfacePort        = Just 55555
                       , cfgRandomizerEnabled    = False
                       , cfgRandomizerStartValue = 0xFF
                       , cfgSCID                 = mkSCID 0
                       , cfgVCIDs                = [0, 1]
                       , cfgTMFrame              = defaultTMFrameConfig
                       , cfgEpoch                = Free (FreeEpoch 2000 001 00 00 00)
                       , cfgLeapSeconds          = 17
                       , cfgUnknownSPID          = SPID 5071
                       , cfgNCTRS                = [defaultNctrsConfig]
                       , cfgCnC                  = [defaultCncConfig]
                       , cfgEDEN                 = [defaultEdenConfig]
                       , cfgSLE                  = Just defaultSleConfig
                       , cfgNDIU                 = [defaultNdiuConfig]
                       , cfgVerification         = defaultVerifConfig
                       , cfgStoreTMFrames        = True
                       , cfgStorePUSPackets      = True
                       }


getInterfaces :: Config -> [ProtocolInterface]
getInterfaces conf = getNctrs ++ getEden ++ getCnc
  where
    getNctrs = RIO.map (IfNctrs . cfgNctrsID) (cfgNCTRS conf)
    getCnc   = RIO.map (IfCnc . cfgCncID) (cfgCnC conf)
    getEden  = RIO.map (IfEden . cfgEdenID) (cfgEDEN conf)

getDefaultInterface :: Config -> Maybe ProtocolInterface
getDefaultInterface cfg = headMaybe (getInterfaces cfg)

getDefaultInterfaceName :: Config -> ShortText
getDefaultInterfaceName cfg = case headMaybe (cfgNCTRS cfg) of
    Just x  -> ST.fromText $ cfgNctrsName x
    Nothing -> case headMaybe (cfgEDEN cfg) of
        Just x  -> ST.fromText $ cfgEdenName x
        Nothing -> case headMaybe (cfgCnC cfg) of
            Just x  -> ST.fromText $ (cfgCncName x)
            Nothing -> "NCTRS A"


getInterfaceMap :: Config -> HashMap ShortText ProtocolInterface
getInterfaceMap cfg = HM.fromList (getNctrs ++ getEden ++ getCnc)
  where
    getNctrs = RIO.map
        (\x -> (ST.fromText (cfgNctrsName x), IfNctrs (cfgNctrsID x)))
        (cfgNCTRS cfg)
    getCnc = RIO.map
        (\x -> (ST.fromText (cfgCncName x), IfCnc (cfgCncID x)))
        (cfgCnC cfg)
    getEden = RIO.map
        (\x -> (ST.fromText (cfgEdenName x), IfEden (cfgEdenID x)))
        (cfgEDEN cfg)



-- | write the config as a serialized string to a file. Uses the Show class for serizalization
-- writeConfigString :: MonadIO m => Config -> FilePath -> m ()
-- writeConfigString cfg path = do
--     liftIO $ Prelude.writeFile path (show cfg)

-- | write the config in JSON format to a file. Uses the aeson for conversion to/from JSON
writeConfigJSON :: MonadIO m => Config -> FilePath -> m ()
writeConfigJSON cfg path = liftIO $ encodeFile path cfg

-- | Load a config from a file in String format (Show/Read instance) and return it.
-- | If there is an error on parsing, return 'Left error'
-- loadConfigString :: MonadIO m => FilePath -> m (Either Text Config)
-- loadConfigString path = do
--     content <- liftIO $ Prelude.readFile path
--     let res = reads content
--     if Prelude.null res
--         then return $ Left ("Could not parse config: " <> T.pack content)
--         else return $ Right . fst . Prelude.head $ res

-- | Load a config from a file in JSON format and return it.
-- | If there is an error on parsing, return 'Left error'
loadConfigJSON :: MonadIO m => FilePath -> m (Either Text Config)
loadConfigJSON path = do
    content <- liftIO $ B.readFile path
    case eitherDecode content of
        Left  err -> return $ Left (T.pack err)
        Right cfg -> return $ Right cfg
