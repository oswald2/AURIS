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

import           Data.ByteString.Lazy          as B
import           Data.ByteString.Lazy.Char8    as BC
import           Data.Text                     as T
import           Data.Text.Short                ( ShortText )
import           Data.Csv
import           Data.Char
import           Data.Vector                   as V

import           System.FilePath
import           System.Directory


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


myOptions :: DecodeOptions
myOptions = defaultDecodeOptions { decDelimiter = fromIntegral (ord '\t') }

fileName :: FilePath
fileName = "plf.dat"


loadFromFile :: FilePath -> IO (Either Text (Vector PLFentry))
loadFromFile mibPath = do
  let file = mibPath </> fileName
  ex <- doesFileExist file
  if ex 
    then do
      content <- B.readFile file
      case decodeWith myOptions NoHeader (BC.filter isAscii content) of
        Left  err -> pure $ Left (T.pack err)
        Right x   -> pure $ Right x
    else do
      return $! Left $ "File " <> T.pack file <> " does not exist."
