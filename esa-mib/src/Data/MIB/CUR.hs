{-# LANGUAGE
    OverloadedStrings
    , BangPatterns
    , NoImplicitPrelude
#-}
module Data.MIB.CUR
  ( CURentry(..)
  , loadFromFile
  )
where

import           RIO

import qualified RIO.Vector                    as V
import           Data.Text.Short                ( ShortText )
import           Data.Csv

import           Data.MIB.Load


data CURentry = CURentry {
    _curParamName :: !ShortText
    , _curPos :: !Int
    , _curRlChk :: !ShortText
    , _curValPar :: !Int
    , _curSelect :: !ShortText
} deriving (Eq, Show)





instance FromRecord CURentry where
  parseRecord v
    | V.length v == 5
    = CURentry <$> v .! 0 <*> v .! 1 <*> v .! 2 <*> v .! 3 <*> v .! 4
    | otherwise
    = mzero



fileName :: FilePath
fileName = "cur.dat"


loadFromFile
  :: (MonadIO m, MonadReader env m, HasLogFunc env)
  => FilePath
  -> m (Either Text (Vector CURentry))
loadFromFile mibPath = loadFromFileGen mibPath fileName


