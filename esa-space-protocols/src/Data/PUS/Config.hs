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
    , VerificationConfig(..)
    , cltuBlockSizeAsWord8
    , defaultConfig
    -- , writeConfigString
    , writeConfigJSON
    -- , loadConfigString
    , loadConfigJSON
    , getInterfaces
    , getDefaultInterface
    , getDefaultInterfaceName
    , getInterfaceMap
    ) where


import           RIO
import           RIO.List                       ( headMaybe )
import qualified RIO.HashMap                   as HM
import           Data.Aeson
import           Data.ByteString.Lazy          as B
import qualified Data.Text                     as T
import           Data.Text.Short                ( ShortText )
import qualified Data.Text.Short               as ST

import           General.PUSTypes

import           General.Time

import           Protocol.ProtocolInterfaces

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
      cfgCncID     :: !Word16
  -- | A name for the connection to be displayed in the GUI
    , cfgCncName   :: !Text
  -- | The host where to connect to 
    , cfgCncHost   :: !Text
  -- | The TM port to connect to 
    , cfgCncPortTM :: !Word16
  -- | The TC port to connect to 
    , cfgCncPortTC :: !Word16
    -- | Configures, if packets on the C&C protocol link should 
    -- be CRC checked or not 
    , cfgCncHasCRC :: !Bool
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


-- | The configuration of the PUS functionality
data Config = Config
    {
    -- | The block size that is used to encode/decode the CLTU
      cfgCltuBlockSize        :: !CltuBlockSize
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
defaultCncConfig = CncConfig { cfgCncID     = 1
                             , cfgCncName   = "SCOE 1"
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
                       , cfgTMFrame              = defaultTMFrameConfig
                       , cfgEpoch                = UnixTime
                       , cfgLeapSeconds          = 17
                       , cfgUnknownSPID          = SPID 5071
                       , cfgNCTRS                = [defaultNctrsConfig]
                       , cfgCnC                  = [defaultCncConfig]
                       , cfgEDEN                 = [defaultEdenConfig]
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
