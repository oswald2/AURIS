{-# LANGUAGE
    OverloadedStrings
    , BangPatterns
    , NoImplicitPrelude
    , DataKinds
#-}
module Data.MIB.LGF
  ( LGFentry(..)
  , loadFromFile
  )
where

import           RIO

import           Data.Text.Short                ( ShortText )
import           Data.Csv

import           Data.MIB.Load
import           Data.MIB.Types


data LGFentry = LGFentry {
    _lgfIdent :: !ShortText
    , _lgfDescr :: !ShortText
    , _lgfPol1 :: ShortTextDefaultTo "0"
    , _lgfPol2 :: ShortTextDefaultTo "0"
    , _lgfPol3 :: ShortTextDefaultTo "0"
    , _lgfPol4 :: ShortTextDefaultTo "0"
    , _lgfPol5 :: ShortTextDefaultTo "0"
} deriving (Eq, Show)



instance FromRecord LGFentry where
  parseRecord = genericParse (>= 7) LGFentry


fileName :: FilePath
fileName = "lgf.dat"


loadFromFile
  :: (MonadIO m, MonadReader env m, HasLogFunc env)
  => FilePath
  -> m (Either Text (Vector LGFentry))
loadFromFile mibPath = loadFromFileGen mibPath fileName
