module Data.MIB.GPC
  ( GPCentry(..)
  , loadFromFile
  )
where

import           RIO

import qualified RIO.Vector                    as V
import           Data.Text.Short                ( ShortText )
import           Data.Csv

import           Data.MIB.Load
import           Data.MIB.Types

data GPCentry = GPCentry {
    _gpcName :: !ShortText
    , _gpcPos :: !Int
    , _gpcWhere :: !Char
    , _gpcParamName :: !ShortText 
    , _gpcRaw :: CharDefaultTo "U"
    , _gpcMinimum :: !ShortText
    , _gpcMaximum :: !ShortText
    , _gpcColor :: !Char
    , _gpcSymbol :: CharDefaultTo "0"
    , _gpcLine :: CharDefaultTo "0"
    , _gpcDomain :: Maybe Int 
} deriving (Eq, Show)



instance Ord GPCentry where
  compare x1 x2 = compare (_gpcName x1) (_gpcName x2)

instance FromRecord GPCentry where
  parseRecord v
    | V.length v == 11
    = GPCentry
      <$> v
      .!  0
      <*> v
      .!  1
      <*> v
      .!  2
      <*> v
      .!  3
      <*> v
      .!  4
      <*> v
      .!  5
      <*> v
      .!  6
      <*> v
      .!  7
      <*> v
      .!  8
      <*> v
      .!  9
      <*> v
      .!  10
    | otherwise
    = mzero



fileName :: FilePath
fileName = "gpc.dat"


loadFromFile
  :: (MonadIO m, MonadReader env m, HasLogFunc env)
  => FilePath
  -> m (Either Text (Vector GPCentry))
loadFromFile mibPath = loadFromFileGen mibPath fileName


