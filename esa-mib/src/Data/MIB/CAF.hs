module Data.MIB.CAF
  ( CAFentry(..)
  , loadFromFile
  )
where

import           RIO

import qualified RIO.Vector                    as V

import           Data.Text.Short                ( ShortText )
import           Data.Csv

import           Data.MIB.Load
import Data.MIB.Types


data CAFentry = CAFentry {
    _cafNumbr :: !ShortText
    , _cafDescr :: !ShortText
    , _cafEngFmt :: !Char
    , _cafRawFmt :: !Char
    , _cafRadix :: !Char
    , _cafUnit :: !ShortText
    , _cafNCurve :: !Int
    , _cafInter :: CharDefaultTo "F"
} deriving (Eq, Show)





instance FromRecord CAFentry where
  parseRecord v
    | V.length v == 8
    = CAFentry
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
      | V.length v == 7
      = CAFentry
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
        <*> pure (CharDefaultTo 'F')
      | otherwise
    = mzero



fileName :: FilePath
fileName = "caf.dat"


loadFromFile
  :: (MonadIO m, MonadReader env m, HasLogFunc env)
  => FilePath
  -> m (Either Text (Vector CAFentry))
loadFromFile mibPath = loadFromFileGen mibPath fileName
