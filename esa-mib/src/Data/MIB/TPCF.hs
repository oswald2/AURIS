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

import           Data.Text.Short                ( ShortText )
import           Data.Csv
import qualified RIO.Vector                    as V
import qualified RIO.HashMap                   as HM

import           Data.MIB.Load


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



fileName :: FilePath
fileName = "tpcf.dat"


loadFromFile
  :: (MonadIO m, MonadReader env m, HasLogFunc env, HasCallStack)
  => FilePath
  -> m (Either Text (Vector TPCFentry))
loadFromFile mibPath = loadFromFileGen mibPath fileName


getTPCFMap :: Vector TPCFentry -> HashMap Word32 TPCFentry
getTPCFMap = V.foldl (\m e -> HM.insert (tpcfSPID e) e m) HM.empty
