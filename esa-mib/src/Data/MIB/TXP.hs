{-# LANGUAGE
    OverloadedStrings
    , BangPatterns
    , NoImplicitPrelude
#-}
module Data.MIB.TXP
  ( TXPentry(..)
  , loadFromFile
  )
where

import           RIO

import           Data.Text.Short                ( ShortText )
import           Data.Csv

import           Data.MIB.Load


data TXPentry = TXPentry {
    _txpNumbr :: !ShortText
    , _txpFrom :: !ShortText
    , _txpTo :: !ShortText
    , _txpAlTxt :: !ShortText
} deriving (Eq, Show)





instance FromRecord TXPentry where
  parseRecord = genericParse (== 4) TXPentry


fileName :: FilePath
fileName = "txp.dat"


loadFromFile
  :: (MonadIO m, MonadReader env m, HasLogFunc env)
  => FilePath
  -> m (Either Text (Vector TXPentry))
loadFromFile mibPath = loadFromFileGen mibPath fileName
