{-|
Module      : Data.PUS.Config
Description : Configuration for the PUS functionality
Copyright   : (c) Michael Oswald, 2019
License     : BSD-3
Maintainer  : michael.oswald@onikudaki.net
Stability   : experimental
Portability : POSIX

This module contains the data structure for configuration. The configuration can currently be read and written from files in native Haskell 
format (via Show class) or as JSON (via the aeson library)
-}
{-# LANGUAGE OverloadedStrings
    , DeriveGeneric 
#-}
module Data.PUS.Config
    (
        Config(..)
        , defaultConfig
        , writeConfigString
        , writeConfigJSON
        , loadConfigString
        , loadConfigJSON
    )
where

import Control.Monad.IO.Class

import Data.Word   
import Data.Aeson
import Data.ByteString.Lazy as B
import Data.Text (Text)
import qualified Data.Text as T

import GHC.Generics

-- | The configuration of the PUS functionality
data Config = Config {
    -- | The block size that is used to encode/decode the CLTU
    cfgCltuBlockSize :: Word8
    , cfgRandomizerStartValue :: Word8
} deriving (Eq, Read, Show, Generic)

instance FromJSON Config

instance ToJSON Config where
    toEncoding = genericToEncoding defaultOptions

-- | a default configuration with typical values.
defaultConfig :: Config 
defaultConfig = Config {
        cfgCltuBlockSize = 8
        , cfgRandomizerStartValue = 0xFF
    }

-- | write the config as a serialized string to a file. Uses the Show class for serizalization
writeConfigString :: MonadIO m => Config -> FilePath -> m ()
writeConfigString cfg path = do 
    liftIO $ Prelude.writeFile path (show cfg)

-- | write the config in JSON format to a file. Uses the aeson for conversion to/from JSON
writeConfigJSON :: MonadIO m => Config -> FilePath -> m ()
writeConfigJSON cfg path = liftIO $ encodeFile path cfg

-- | Load a config from a file in String format (Show/Read instance) and return it. 
-- | If there is an error on parsing, return 'Left error'
loadConfigString :: MonadIO m => FilePath -> m (Either Text Config)
loadConfigString path = do
    content <- liftIO $ Prelude.readFile path
    let res = reads content
    if Prelude.null res 
        then return  $ Left ("Could not parse config: " <> T.pack content)
        else return $ Right . fst . Prelude.head $ res

-- | Load a config from a file in JSON format and return it. 
-- | If there is an error on parsing, return 'Left error'
loadConfigJSON :: MonadIO m => FilePath -> m (Either Text Config)
loadConfigJSON path = do
    content <- liftIO $ B.readFile path
    case eitherDecode content of
        Left err -> return $ Left (T.pack err)
        Right cfg -> return $ Right cfg