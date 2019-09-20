{-# LANGUAGE
    OverloadedStrings
    , BangPatterns
    , NoImplicitPrelude
#-}
module Data.MIB.TPCF
  ( TPCFentry(..)
  , loadFromFile
  , getTPCFMap
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
import           RIO.HashMap                   as HM

import           System.FilePath
import           System.Directory


data TPCFentry = TPCFentry {
    tpcfSPID :: !Word32,
    tpcfName :: !ShortText,
    tpcfSize :: Maybe Int
} deriving Show


instance Eq TPCFentry where
  f1 == f2 = tpcfSPID f1 == tpcfSPID f2



instance FromRecord TPCFentry where
  parseRecord v | V.length v == 3 = TPCFentry <$> v .! 0 <*> v .! 1 <*> v .! 2
                | otherwise       = mzero



myOptions :: DecodeOptions
myOptions = defaultDecodeOptions { decDelimiter = fromIntegral (ord '\t') }

fileName :: FilePath
fileName = "tpcf.dat"


loadFromFile :: FilePath -> IO (Either Text (Vector TPCFentry))
loadFromFile mibPath = do
  let file = mibPath </> fileName
  ex <- doesFileExist file
  if ex 
    then do
      content <- B.readFile file
      case decodeWith myOptions NoHeader (BC.filter isAscii content) of
        Left  err -> pure $ Left (T.pack err)
        Right x   -> pure (Right x)
    else do
      return $! Left $ "File " <> T.pack file <> " does not exist."

getTPCFMap :: Vector TPCFentry -> HashMap Word32 TPCFentry
getTPCFMap = V.foldl (\m e -> HM.insert (tpcfSPID e) e m) HM.empty
