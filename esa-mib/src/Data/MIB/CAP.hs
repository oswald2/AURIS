{-# LANGUAGE
      BangPatterns
    , NoImplicitPrelude
#-}
module Data.MIB.CAP
  ( CAPentry(..)
  , loadFromFile
  )
where

import           RIO

import           Data.Text.Short                ( ShortText )
import           Data.Csv

import           Data.MIB.Load


data CAPentry = CAPentry {
    _capNumbr :: !ShortText
    , _capXVals :: !ShortText
    , _capYVals :: !ShortText
} deriving (Eq, Show)


instance FromRecord CAPentry where
  parseRecord = genericParse (== 3) CAPentry



fileName :: FilePath
fileName = "cap.dat"


loadFromFile
  :: (MonadIO m, MonadReader env m, HasLogFunc env)
  => FilePath
  -> m (Either Text (Vector CAPentry))
loadFromFile mibPath = loadFromFileGen mibPath fileName
