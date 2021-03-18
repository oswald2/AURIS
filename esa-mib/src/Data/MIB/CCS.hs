{-# LANGUAGE
    TemplateHaskell
#-}
module Data.MIB.CCS
    ( CCSentry(..)
    , loadFromFile
    , ccsNumbr
    , ccsEng  
    , ccsRaw  
    )
where 

import           RIO
import           Control.Lens                   ( makeLenses )

import           Data.Text.Short                ( ShortText )
import           Data.Csv

import           Data.MIB.Load


data CCSentry = CCSentry
    { _ccsNumbr :: !ShortText
    , _ccsEng   :: !ShortText
    , _ccsRaw   :: !ShortText
    }
    deriving Show
makeLenses ''CCSentry

instance Eq CCSentry where
    f1 == f2 = _ccsNumbr f1 == _ccsNumbr f2



instance FromRecord CCSentry where
    parseRecord = genericParse (== 3) CCSentry


fileName :: FilePath
fileName = "ccs.dat"


loadFromFile
    :: (MonadIO m, MonadReader env m, HasLogFunc env, HasCallStack)
    => FilePath
    -> m (Either Text (Vector CCSentry))
loadFromFile mibPath = loadFromFileGen mibPath fileName


