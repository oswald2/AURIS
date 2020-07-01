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

import           Data.Text.Short                ( ShortText )
import           Data.Csv

import           Data.MIB.Load


data TXFentry = TXFentry {
    _txfNumbr :: !ShortText
    , _txfDescr :: !ShortText
    , _txfRawFmt :: !Char
    , _txfNAlias :: Maybe Int
} deriving (Eq, Show)



instance FromRecord TXFentry where
  parseRecord = genericParse (== 4) TXFentry


fileName :: FilePath
fileName = "txf.dat"


loadFromFile
  :: (MonadIO m, MonadReader env m, HasLogFunc env)
  => FilePath
  -> m (Either Text (Vector TXFentry))
loadFromFile mibPath = loadFromFileGen mibPath fileName
