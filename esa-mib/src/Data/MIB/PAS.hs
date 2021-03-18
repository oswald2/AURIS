{-# LANGUAGE
    TemplateHaskell
#-}
module Data.MIB.PAS
    ( PASentry(..)
    , loadFromFile
    , pasNumbr
    , pasEng  
    , pasRaw  
    )
where 

import           RIO
import           Control.Lens                   ( makeLenses )

import           Data.Text.Short                ( ShortText )
import           Data.Csv

import           Data.MIB.Load


data PASentry = PASentry {
     _pasNumbr :: !ShortText
    , _pasEng   :: !ShortText
    , _pasRaw   :: !ShortText
    }
    deriving Show
makeLenses ''PASentry

instance Eq PASentry where
    f1 == f2 = _pasNumbr f1 == _pasNumbr f2



instance FromRecord PASentry where
    parseRecord = genericParse (== 3) PASentry


fileName :: FilePath
fileName = "pas.dat"


loadFromFile
    :: (MonadIO m, MonadReader env m, HasLogFunc env, HasCallStack)
    => FilePath
    -> m (Either Text (Vector PASentry))
loadFromFile mibPath = loadFromFileGen mibPath fileName


