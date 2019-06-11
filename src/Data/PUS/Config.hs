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
{-# LANGUAGE OverloadedStrings
    , DeriveGeneric
    , DataKinds
    , TypeSynonymInstances
    , FlexibleInstances
    , GeneralizedNewtypeDeriving
#-}
module Data.PUS.Config
    (
    -- | The config data type itself
      Config(..)
    -- | Type for the CLTU code block size. Restricted to be 5 .. 8
    , CltuBlockSize(..)
    , cltuBlockSizeAsWord8
    , defaultConfig
    -- , writeConfigString
    , writeConfigJSON
    -- , loadConfigString
    , loadConfigJSON
    )
where

import           Control.Monad.IO.Class

import           Data.Word
import           Data.Aeson
import           Data.ByteString.Lazy          as B
import           Data.Text                      ( Text )
import qualified Data.Text                     as T

import           Closed

import           GHC.Generics

import           Data.PUS.Types



-- | The configuration of the PUS functionality
data Config = Config {
    -- | The block size that is used to encode/decode the CLTU
    cfgCltuBlockSize :: CltuBlockSize
    -- | If the socket interface is used, specifies Just portnumber, else Nothing
    , cfgInterfacePort :: Maybe Word16
    -- | The start value of the randomizer used for standard ESA TC randomization
    , cfgRandomizerStartValue :: Word8
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


-- | a default configuration with typical values.
defaultConfig :: Config
defaultConfig = Config
    { cfgCltuBlockSize        = CltuBS_8
    , cfgInterfacePort        = Just 55555
    , cfgRandomizerStartValue = 0xFF
    , cfgSCID                 = mkSCID 0
    , cfgVCIDs                = [0, 1]
    , cfgMaxTMFrameLen        = 1115
    , cfgTMFrameHasCRC        = True
    }

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
