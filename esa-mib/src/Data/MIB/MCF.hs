{-# LANGUAGE
    OverloadedStrings
    , BangPatterns
    , NoImplicitPrelude
    , DataKinds
#-}
module Data.MIB.MCF
  ( MCFentry(..)
  , loadFromFile
  )
where

import           RIO

import           Data.Text.Short                ( ShortText )
import           Data.Csv

import           Data.MIB.Load
import           Data.MIB.Types


data MCFentry = MCFentry {
    _mcfIdent :: !ShortText
    , _mcfDescr :: !ShortText
    , _mcfPol1 :: ShortTextDefaultTo "0"
    , _mcfPol2 :: ShortTextDefaultTo "0"
    , _mcfPol3 :: ShortTextDefaultTo "0"
    , _mcfPol4 :: ShortTextDefaultTo "0"
    , _mcfPol5 :: ShortTextDefaultTo "0"
} deriving (Eq, Show)


instance FromRecord MCFentry where
  parseRecord = genericParse (== 7) MCFentry


fileName :: FilePath
fileName = "mcf.dat"


loadFromFile
  :: (MonadIO m, MonadReader env m, HasLogFunc env)
  => FilePath
  -> m (Either Text (Vector MCFentry))
loadFromFile mibPath = loadFromFileGen mibPath fileName
