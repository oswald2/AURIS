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
  , cltuBlockSizeAsWord8
  , defaultConfig
    -- , writeConfigString
  , writeConfigJSON
    -- , loadConfigString
  , loadConfigJSON
  , getInterfaces
  )
where


import           RIO

import           Data.Aeson
import           Data.ByteString.Lazy          as B
import qualified Data.Text                     as T

import           Closed

import           General.PUSTypes

import           General.Time

import           Protocol.ProtocolInterfaces


data NctrsConfig = NctrsConfig {
  cfgNctrsID :: Word16
  , cfgNctrsHost :: Text
  , cfgNctrsPortTC :: Word16
  , cfgNctrsPortTM :: Word16
  , cfgNctrsPortADM :: Word16
} deriving (Eq, Generic)

instance FromJSON NctrsConfig
instance ToJSON NctrsConfig where
  toEncoding = genericToEncoding defaultOptions


data CncConfig = CncConfig {
  -- | A numerical ID, which needs to be unique for this C&C connection
  cfgCncID :: Word16
  -- | The host where to connect to 
  , cfgCncHost :: Text
  -- | The TM port to connect to 
  , cfgCncPortTM :: Word16
  -- | The TC port to connect to 
  , cfgCncPortTC :: Word16
    -- | Configures, if packets on the C&C protocol link should 
    -- be CRC checked or not 
  , cfgCncHasCRC :: Bool
} deriving (Eq, Generic)

instance FromJSON CncConfig
instance ToJSON CncConfig where
  toEncoding = genericToEncoding defaultOptions


-- | Configuration for an EDEN connection. 
data EDENConfig = EDENConfig {
    -- | A numerical ID, which needs to be unique for this C&C connection
    cfgEdenID :: Word16
    -- | The host where to connect to 
    , cfgEdenHost :: Text
    -- | The port where to connect to 
    , cfgEdenPort :: Word16
    }
    deriving (Eq, Generic)

instance FromJSON EDENConfig
instance ToJSON EDENConfig where
  toEncoding = genericToEncoding defaultOptions


-- | The configuration of the PUS functionality
data Config = Config {
    -- | The block size that is used to encode/decode the CLTU
    cfgCltuBlockSize :: CltuBlockSize
    -- | If the socket interface is used, specifies Just portnumber, else Nothing
    , cfgInterfacePort :: Maybe Word16
    -- | If the TC randomization is enabled by default
    , cfgRandomizerEnabled :: !Bool
    -- | The start value of the randomizer used for standard ESA TC randomization
    , cfgRandomizerStartValue :: !Word8
    -- | The spacecraft ID used
    , cfgSCID :: SCID
    -- | A list of available virtual channels.
    , cfgVCIDs :: [VCID]
    -- | The maximum TM Frame length. This length is used in parsing
    -- the frame data, so it needs to be accurate. Default is by
    -- PUS Standard a value 1115 (1024 bytes data)
    , cfgMaxTMFrameLen :: Closed 128 2040
    -- | Indicates, if a TM frame does contain a CRC value
    , cfgTMFrameHasCRC :: Bool
    -- | The configured segment length for TM Frames
    , cfgTMSegLength :: !TMSegmentLen
    -- | Specifies the time epoch to be used for time handling
    , cfgEpoch :: EpochType
    -- | Specified the used leap seconds
    , cfgLeapSeconds :: LeapSeconds
    -- | Packets, which cannot be identified will be given this 
    -- SPID
    , cfgUnknownSPID :: SPID
    -- | Specifies the configurations for the available NCTRS connections
    , cfgNCTRS :: [NctrsConfig]
    -- | Specifies the configurations of the available C&C connections
    , cfgCnC :: [CncConfig]
    -- | Specifies the configuration of the available EDEN connections
    , cfgEDEN :: [EDENConfig]
} deriving (Eq, Generic)


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
                               , cfgEdenHost = "localhost"
                               , cfgEdenPort = 40300
                               }


defaultNctrsConfig :: NctrsConfig
defaultNctrsConfig = NctrsConfig { cfgNctrsID      = 1
                                 , cfgNctrsHost    = "localhost"
                                 , cfgNctrsPortTC  = 20009
                                 , cfgNctrsPortTM  = 2502
                                 , cfgNctrsPortADM = 20010
                                 }

defaultCncConfig :: CncConfig
defaultCncConfig = CncConfig { cfgCncID     = 1
                             , cfgCncHost   = "localhost"
                             , cfgCncPortTM = 10000
                             , cfgCncPortTC = 11000
                             , cfgCncHasCRC = True
                             }


-- | a default configuration with typical values.
defaultConfig :: Config
defaultConfig = Config { cfgCltuBlockSize        = CltuBS_8
                       , cfgInterfacePort        = Just 55555
                       , cfgRandomizerEnabled    = False 
                       , cfgRandomizerStartValue = 0xFF
                       , cfgSCID                 = mkSCID 0
                       , cfgVCIDs                = [0, 1]
                       , cfgMaxTMFrameLen        = 1115
                       , cfgTMFrameHasCRC        = True
                       , cfgTMSegLength          = TMSegment65536
                       , cfgEpoch                = UnixTime
                       , cfgLeapSeconds          = 17
                       , cfgUnknownSPID          = SPID 5071
                       , cfgNCTRS                = [defaultNctrsConfig]
                       , cfgCnC                  = [defaultCncConfig]
                       , cfgEDEN                 = [defaultEdenConfig]
                       }


getInterfaces :: Config -> [ProtocolInterface]
getInterfaces conf = getNctrs ++ getCnc ++ getEden
  where 
    getNctrs = RIO.map (IfNctrs . cfgNctrsID) (cfgNCTRS conf)
    getCnc = RIO.map (IfCnc . cfgCncID) (cfgCnC conf)
    getEden = RIO.map (IfEden . cfgEdenID) (cfgEDEN conf)


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
