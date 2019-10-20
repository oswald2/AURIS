{-|
Module      : Data.TM.Synthetic
Description : Module for handling synthetic parameters
Copyright   : (c) Michael Oswald, 2019
License     : BSD-3
Maintainer  : michael.oswald@onikudaki.net
Stability   : experimental
Portability : POSIX

This module is for hanlding synthetic parameters. Currently, only
the OL language of SCOS will be supported. Future implementations might
include SPEL or PLUTO.
-}
module Data.TM.Synthetic
    ( SyntheticAST(..)
    , Synthetic(..)
    , parseOL
    )
where

import           RIO
import qualified Data.Text.IO                  as T
                                                ( readFile )
import           Codec.Serialise
import           Data.Aeson

-- | The abstract syntax tree for the calculations
data SyntheticAST = SyntheticAST
    deriving(Show, Generic)


instance NFData SyntheticAST
instance Serialise SyntheticAST
instance FromJSON SyntheticAST
instance ToJSON SyntheticAST where
    toEncoding = genericToEncoding defaultOptions

-- | Describe a synthetic parameter.
data Synthetic = Synthetic {
    -- | The script source code itself
    _synthScript :: Text
    -- | The parsed representation of the code
    , _synthAST :: SyntheticAST
    } deriving (Show, Generic)

instance NFData Synthetic
instance Serialise Synthetic
instance FromJSON Synthetic
instance ToJSON Synthetic where
    toEncoding = genericToEncoding defaultOptions



-- | Read the content of a file which is assumed to be a OL expression.
-- Returns either an error message or the 'Synthetic' parameter itself
parseOL :: (MonadIO m) => FilePath -> m (Either Text Synthetic)
parseOL fileName = do
    content <- liftIO $ T.readFile fileName
    pure (Right (Synthetic content SyntheticAST))


