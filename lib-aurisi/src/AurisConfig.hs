{-# LANGUAGE
    OverloadedStrings
    , BangPatterns
    , NoImplicitPrelude
    , DeriveGeneric
#-}
module AurisConfig
    ( AurisConfig(..)
    , AurisTheme(..)
    , AurisConfig.writeConfigJSON
    , AurisConfig.loadConfigJSON
    , AurisConfig.defaultConfig
    , defaultConfigFileName
    , convLogLevel
    , configPath
    , configPathInstance
    , configPretty
    , defaultMIBFile
    ) where

import           RIO
import qualified RIO.ByteString.Lazy           as B
import qualified RIO.Text                      as T
import           Data.Aeson
import           Data.Aeson.Encode.Pretty       ( encodePretty )

import           Data.PUS.Config
import           System.FilePath

import           Data.DbConfig.MongoDB


configPath :: FilePath
configPath = ".config/AURISi"

configPathInstance :: Word32 -> FilePath 
configPathInstance instanceNr = ".config/AURISi_" ++ show instanceNr


defaultMIBFile :: FilePath
defaultMIBFile = configPath </> "data_model.raw"



data ConfigLogLevel =
  LogLevelDebug
  | LogLevelInfo
  | LogLevelWarn
  | LogLevelError
  | LogLevelOther !Text
  deriving (Eq, Show, Read, Generic)

instance FromJSON ConfigLogLevel
instance ToJSON ConfigLogLevel where
    toEncoding = genericToEncoding defaultOptions

convLogLevel :: ConfigLogLevel -> LogLevel
convLogLevel LogLevelDebug     = LevelDebug
convLogLevel LogLevelInfo      = LevelInfo
convLogLevel LogLevelWarn      = LevelWarn
convLogLevel LogLevelError     = LevelError
convLogLevel (LogLevelOther x) = LevelOther x


data AurisTheme = ThemeDark | ThemeLight
    deriving(Eq, Ord, Enum, Show, Read, Generic)

instance FromJSON AurisTheme
instance ToJSON AurisTheme where
    toEncoding = genericToEncoding defaultOptions


data AurisConfig = AurisConfig
    { aurisMission   :: Text
    , aurisMIB       :: Maybe Text
    , aurisLogLevel  :: ConfigLogLevel
    , aurisPusConfig :: Config
    -- | Minimum level of log messages that should be stored to database.
    -- Set 'Nothing' to disable logging to database.
    , aurisDbConfig  :: Maybe DbConfigMongoDB
    , aurisTheme     :: AurisTheme 
    , aurisInstance  :: !Word32
    }
    deriving (Eq, Generic)


defaultConfig :: AurisConfig
defaultConfig = AurisConfig { aurisPusConfig = Data.PUS.Config.defaultConfig
                            , aurisMission   = "DEFAULT"
                            , aurisLogLevel  = LogLevelInfo
                            , aurisMIB       = Nothing
                            , aurisDbConfig  = Just defaultMongoDBConfig
                            , aurisTheme     = ThemeLight
                            , aurisInstance  = 0
                            }

defaultConfigFileName :: FilePath
defaultConfigFileName = "AURISi.cfg"

instance FromJSON AurisConfig
instance ToJSON AurisConfig where
    toEncoding = genericToEncoding defaultOptions


configPretty :: AurisConfig -> Text
configPretty cfg = case (decodeUtf8' . B.toStrict . encodePretty) cfg of
    Left  err -> "Error decoding Config in UTF8: " <> T.pack (show err)
    Right val -> val


-- | write the config in JSON format to a file. Uses the aeson for conversion to/from JSON
writeConfigJSON :: MonadIO m => AurisConfig -> FilePath -> m ()
writeConfigJSON cfg path = liftIO $ B.writeFile path (encodePretty cfg)


-- | Load a config from a file in JSON format and return it.
-- | If there is an error on parsing, return 'Left error'
loadConfigJSON :: MonadIO m => FilePath -> m (Either Text AurisConfig)
loadConfigJSON path = do
    content <- liftIO $ B.readFile path
    case eitherDecode content of
        Left  err -> return $ Left (T.pack err)
        Right cfg -> return $ Right cfg
