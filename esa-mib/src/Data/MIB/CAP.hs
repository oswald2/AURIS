{-# LANGUAGE
    OverloadedStrings
    , BangPatterns
    , NoImplicitPrelude
#-}
module Data.MIB.CAP
  ( CAPentry(..)
  , loadFromFile
  )
where

import           RIO

import qualified RIO.Vector                    as V

import           Data.Text.Short                ( ShortText )
import           Data.Csv

import           Data.MIB.Load


data CAPentry = CAPentry {
    _capNumbr :: !ShortText
    , _capXVals :: !ShortText
    , _capYVals :: !ShortText
} deriving (Eq, Show)





instance FromRecord CAPentry where
  parseRecord v
    | V.length v == 3
    = CAPentry
      <$> v
      .!  0
      <*> v
      .!  1
      <*> v
      .!  2
    | otherwise
    = mzero



fileName :: FilePath
fileName = "cap.dat"


loadFromFile
  :: (MonadIO m, MonadReader env m, HasLogFunc env)
  => FilePath
  -> m (Either Text (Vector CAPentry))
loadFromFile mibPath = loadFromFileGen mibPath fileName
