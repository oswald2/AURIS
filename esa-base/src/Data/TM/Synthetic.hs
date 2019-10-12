module Data.TM.Synthetic
    ( SyntheticAST(..)
    , Synthetic(..)
    , parseOL
    )
where

import           RIO
import qualified Data.Text.IO                  as T
                                                ( readFile )



data SyntheticAST = SyntheticAST
    deriving(Show, Generic)


data Synthetic = Synthetic {
    _synthScript :: Text
    , _synthAST :: SyntheticAST
    } deriving (Show, Generic)


parseOL :: (MonadIO m) => FilePath -> m (Either Text Synthetic)
parseOL fileName = do
    content <- liftIO $ T.readFile fileName
    pure (Right (Synthetic content SyntheticAST))


