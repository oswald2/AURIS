{-# LANGUAGE
    OverloadedStrings
    , BangPatterns
    , NoImplicitPrelude
#-}
module Data.MIB.LGF
  ( LGFentry(..)
  , loadFromFile
  )
where

import           RIO

import qualified RIO.Vector                    as V

import           Data.Text.Short                ( ShortText )
import           Data.Csv

import           Data.MIB.Load


data LGFentry = LGFentry {
    _lgfIdent :: !ShortText
    , _lgfDescr :: !ShortText
    , _lgfPol1 :: !ShortText
    , _lgfPol2 :: !ShortText
    , _lgfPol3 :: !ShortText
    , _lgfPol4 :: !ShortText
    , _lgfPol5 :: !ShortText
} deriving (Eq, Show)





instance FromRecord LGFentry where
  parseRecord v
    | V.length v == 7
    = LGFentry
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
    | otherwise
    = mzero



fileName :: FilePath
fileName = "lgf.dat"


loadFromFile
  :: (MonadIO m, MonadReader env m, HasLogFunc env)
  => FilePath
  -> m (Either Text (Vector LGFentry))
loadFromFile mibPath = loadFromFileGen mibPath fileName
