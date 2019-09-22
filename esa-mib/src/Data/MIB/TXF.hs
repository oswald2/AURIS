{-# LANGUAGE
    OverloadedStrings
    , BangPatterns
    , NoImplicitPrelude
#-}
module Data.MIB.TXF
  ( TXFentry(..)
  , loadFromFile
  )
where

import           RIO

import qualified RIO.Vector                    as V

import           Data.Text.Short                ( ShortText )
import           Data.Csv

import           Data.MIB.Load


data TXFentry = TXFentry {
    _txfNumbr :: !ShortText
    , _txfDescr :: !ShortText
    , _txfRawFmt :: !Char
    , _txfNAlias :: !Int
} deriving (Eq, Show)



instance FromRecord TXFentry where
  parseRecord v
    | V.length v == 4
    = TXFentry
      <$> v
      .!  0
      <*> v
      .!  1
      <*> v
      .!  2
      <*> v
      .!  3
    | otherwise
    = mzero



fileName :: FilePath
fileName = "txf.dat"


loadFromFile
  :: (MonadIO m, MonadReader env m, HasLogFunc env)
  => FilePath
  -> m (Either Text (Vector TXFentry))
loadFromFile mibPath = loadFromFileGen mibPath fileName
