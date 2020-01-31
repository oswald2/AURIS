{-# LANGUAGE
    OverloadedStrings
    , BangPatterns
    , NoImplicitPrelude
    , DeriveGeneric
#-}
module AurisConfig
  ( AurisConfig(..)
  , AurisConfig.writeConfigJSON
  , AurisConfig.loadConfigJSON
  , AurisConfig.defaultConfig
  , defaultConfigFileName
  , convLogLevel
  )
where

import           RIO
import qualified RIO.ByteString.Lazy           as B
import qualified RIO.Text                      as T
import           Data.Aeson
import           Data.Aeson.Encode.Pretty       ( encodePretty )

import           Data.PUS.Config


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




data AurisConfig = AurisConfig {
    aurisMission :: Text
    , aurisNctrsHost :: Text
    , aurisNctrsTMPort :: Int
    , aurisNctrsTCPort :: Int
    , aurisNctrsAdminPort :: Int
    , aurisCnCHost :: Text
    , aurisCnCTMPort :: Int
    , aurisCnCTCPort :: Int
    , aurisMIB :: Maybe Text
    , aurisLogLevel :: ConfigLogLevel
    , aurisPusConfig :: Config
    }
    deriving(Eq,Generic)


defaultConfig :: AurisConfig
defaultConfig = AurisConfig { aurisPusConfig = Data.PUS.Config.defaultConfig
                            , aurisMission        = "DEFAULT"
                            , aurisNctrsHost      = "localhost"
                            , aurisNctrsTMPort    = 2502
                            , aurisNctrsTCPort    = 20009
                            , aurisNctrsAdminPort = 20010
                            , aurisCnCHost        = "localhost"
                            , aurisCnCTMPort      = 33333
                            , aurisCnCTCPort      = 22222
                            , aurisLogLevel       = LogLevelInfo
                            , aurisMIB            = Nothing
                            }

defaultConfigFileName :: FilePath
defaultConfigFileName = "AURISi.cfg"

instance FromJSON AurisConfig
instance ToJSON AurisConfig where
  toEncoding = genericToEncoding defaultOptions



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
