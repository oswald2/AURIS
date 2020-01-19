module Data.MIB.GPF
  ( GPFentry(..)
  , loadFromFile
  )
where

import           RIO

import qualified RIO.Vector                    as V
import           Data.Text.Short                ( ShortText )
import           Data.Csv

import           Data.MIB.Load
import           Data.MIB.Types

data GPFentry = GPFentry {
    _gpfName :: !ShortText
    , _gpfType :: !Char
    , _gpfHead :: !ShortText
    , _gpfScroll :: CharDefaultTo "N"
    , _gpfHCopy :: CharDefaultTo "N"
    , _gpfDays :: !Int
    , _gpfHours :: !Int
    , _gpfMinutes :: !Int
    , _gpfAxesColor :: !Char
    , _gpfXTick :: !Word8
    , _gpfYTick :: !Word8
    , _gpfXGrid :: !Word8
    , _gpfYGrid :: !Word8
    , _gpfUpun :: !Word8
} deriving (Eq, Show)



instance Ord GPFentry where
  compare x1 x2 = compare (_gpfName x1) (_gpfName x2)

instance FromRecord GPFentry where
  parseRecord v
    | V.length v == 14
    = GPFentry
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
      <*> v
      .!  11
      <*> v
      .!  12
      <*> v
      .!  13
    | otherwise
    = mzero



fileName :: FilePath
fileName = "gpf.dat"


loadFromFile
  :: (MonadIO m, MonadReader env m, HasLogFunc env)
  => FilePath
  -> m (Either Text (Vector GPFentry))
loadFromFile mibPath = loadFromFileGen mibPath fileName


