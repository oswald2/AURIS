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


data SyntheticAST = SyntheticAST
    deriving(Show, Generic)


instance NFData SyntheticAST
instance Serialise SyntheticAST
instance FromJSON SyntheticAST
instance ToJSON SyntheticAST where
    toEncoding = genericToEncoding defaultOptions


data Synthetic = Synthetic {
    _synthScript :: Text
    , _synthAST :: SyntheticAST
    } deriving (Show, Generic)

instance NFData Synthetic
instance Serialise Synthetic
instance FromJSON Synthetic
instance ToJSON Synthetic where
    toEncoding = genericToEncoding defaultOptions




parseOL :: (MonadIO m) => FilePath -> m (Either Text Synthetic)
parseOL fileName = do
    content <- liftIO $ T.readFile fileName
    pure (Right (Synthetic content SyntheticAST))


