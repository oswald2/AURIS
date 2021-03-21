module Data.MIB.DST
    ( DSTentry(..)
    , loadFromFile
    ) where

import           RIO

import           Data.Text.Short                ( ShortText )
import           Data.Csv

import           Data.MIB.Load


data DSTentry = DSTentry
    { _dstApid :: !Word32 
    , _dstRoute :: !ShortText 
    }
    deriving (Show)

instance Eq DSTentry where
    f1 == f2 = _dstApid f1 == _dstApid f2

instance FromRecord DSTentry where
  parseRecord = genericParse (== 2) DSTentry


fileName :: FilePath
fileName = "dst.dat"


loadFromFile
    :: (MonadIO m, MonadReader env m, HasLogFunc env)
    => FilePath
    -> m (Either Text (Vector DSTentry))
loadFromFile mibPath = loadFromFileGen mibPath fileName
