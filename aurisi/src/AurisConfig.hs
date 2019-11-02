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
  )
where

import           RIO
import qualified RIO.ByteString.Lazy           as B
import qualified RIO.Text                      as T
import           Data.Aeson

import           Data.PUS.Config



data AurisConfig = AurisConfig {
    aurisMission :: Text 
    , aurisNctrsHost :: Text
    , aurisNctrsTMPort :: Int 
    , aurisNctrsTCPort :: Int 
    , aurisNctrsAdminPort :: Int 
    , aurisMIB :: Maybe Text 
    , aurisPusConfig :: Config
    }
    deriving(Eq,Generic)


defaultConfig :: AurisConfig 
defaultConfig = AurisConfig {
        aurisPusConfig = Data.PUS.Config.defaultConfig
        , aurisMission = "DEFAULT"
        , aurisNctrsHost = "localhost"
        , aurisNctrsTMPort = 2502
        , aurisNctrsTCPort = 32111 
        , aurisNctrsAdminPort =32110
        , aurisMIB = Nothing
        }

defaultConfigFileName :: FilePath
defaultConfigFileName = "AURISi.cfg"

instance FromJSON AurisConfig
instance ToJSON AurisConfig where
  toEncoding = genericToEncoding defaultOptions



-- | write the config in JSON format to a file. Uses the aeson for conversion to/from JSON
writeConfigJSON :: MonadIO m => AurisConfig -> FilePath -> m ()
writeConfigJSON cfg path = liftIO $ encodeFile path cfg


-- | Load a config from a file in JSON format and return it.
-- | If there is an error on parsing, return 'Left error'
loadConfigJSON :: MonadIO m => FilePath -> m (Either Text AurisConfig)
loadConfigJSON path = do
  content <- liftIO $ B.readFile path
  case eitherDecode content of
    Left  err -> return $ Left (T.pack err)
    Right cfg -> return $ Right cfg
