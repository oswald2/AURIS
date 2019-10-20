{-# LANGUAGE
    TemplateHaskell
#-}
module Data.MIB.TPCF
  ( TPCFentry(..)
  , loadFromFile
  , getTPCFMap
  , tpcfSPID
  , tpcfName
  , tpcfSize
)
where

import           RIO
import           Control.Lens                   ( makeLenses )

import           Data.Text.Short                ( ShortText )
import           Data.Csv
import qualified RIO.Vector                    as V
import qualified RIO.HashMap                   as HM

import           Data.MIB.Load


data TPCFentry = TPCFentry {
    _tpcfSPID :: !Word32,
    _tpcfName :: !ShortText,
    _tpcfSize :: Maybe Int
} deriving Show
makeLenses ''TPCFentry

instance Eq TPCFentry where
  f1 == f2 = _tpcfSPID f1 == _tpcfSPID f2



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
getTPCFMap = V.foldl (\m e -> HM.insert (_tpcfSPID e) e m) HM.empty
