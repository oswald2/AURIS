{-# LANGUAGE
    OverloadedStrings
    , BangPatterns
    , NoImplicitPrelude
#-}
module Data.MIB.PLF
  ( PLFentry(..)
  , loadFromFile
  )
where

import           RIO

import           Data.Text.Short                ( ShortText )
import           Data.Csv
import qualified RIO.Vector                    as V

import           Data.MIB.Load



data PLFentry = PLFentry {
    plfName :: !ShortText,
    plfSPID :: !Word32,
    plfOffBy :: !Int,
    plfOffBi :: !Int,
    plfNbOcc :: Maybe Int,
    plfLgOcc :: Maybe Int,
    plfTime :: Maybe Int,
    plfTdOcc :: Maybe Int
} deriving (Show, Read)


instance Eq PLFentry where
  plf1 == plf2 = plfName plf1 == plfName plf2


instance Ord PLFentry where
  compare plf1 plf2 =
    let val1 = plfOffBy plf1 * 8 + plfOffBi plf1
        val2 = plfOffBy plf2 * 8 + plfOffBi plf2
    in  compare val1 val2



instance FromRecord PLFentry where
  parseRecord v
    | V.length v >= 8
    = PLFentry
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
      <*> v
      .!  7
    | otherwise
    = mzero


fileName :: FilePath
fileName = "plf.dat"

loadFromFile
  :: (MonadIO m, MonadReader env m, HasLogFunc env, HasCallStack)
  => FilePath
  -> m (Either Text (Vector PLFentry))
loadFromFile mibPath = loadFromFileGen mibPath fileName

