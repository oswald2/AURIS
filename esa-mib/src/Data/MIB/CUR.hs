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

instance Ord CURentry where
    compare x1 x2 = compare (_curPos x1) (_curPos x2)

instance FromRecord CURentry where
  parseRecord = genericParse (== 5) CURentry


fileName :: FilePath
fileName = "cur.dat"


loadFromFile
  :: (MonadIO m, MonadReader env m, HasLogFunc env)
  => FilePath
  -> m (Either Text (Vector CURentry))
loadFromFile mibPath = loadFromFileGen mibPath fileName


