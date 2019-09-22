{-# LANGUAGE
    OverloadedStrings
    , BangPatterns
    , NoImplicitPrelude
#-}
module Data.MIB.MCF
  ( MCFentry(..)
  , loadFromFile
  )
where

import           RIO

import qualified RIO.Vector                    as V

import           Data.Text.Short                ( ShortText )
import           Data.Csv

import           Data.MIB.Load


data MCFentry = MCFentry {
    _mcfIdent :: !ShortText
    , _mcfDescr :: !ShortText
    , _mcfPol1 :: !ShortText
    , _mcfPol2 :: !ShortText
    , _mcfPol3 :: !ShortText
    , _mcfPol4 :: !ShortText
    , _mcfPol5 :: !ShortText
} deriving (Eq, Show)





instance FromRecord MCFentry where
  parseRecord v
    | V.length v == 7
    = MCFentry
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
fileName = "mcf.dat"


loadFromFile
  :: (MonadIO m, MonadReader env m, HasLogFunc env)
  => FilePath
  -> m (Either Text (Vector MCFentry))
loadFromFile mibPath = loadFromFileGen mibPath fileName
