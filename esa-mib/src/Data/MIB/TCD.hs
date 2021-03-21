module Data.MIB.TCD
    ( TCDentry(..)
    , loadFromFile
    , getTCDMap
    ) where

import           RIO
import qualified RIO.HashMap                   as HM
import qualified RIO.Vector                    as V
import           Data.Text.Short                ( ShortText )
import           Data.Csv

import           Data.MIB.Load


data TCDentry = TCDentry
    { _tcdApid :: !Word32
    , _tcdLink :: !ShortText
    , _tcdFlag :: !Char
    }
    deriving (Eq, Ord, Show)

instance FromRecord TCDentry where
    parseRecord = genericParse (== 3) TCDentry


fileName :: FilePath
fileName = "tcd.dat"


loadFromFile
    :: (MonadIO m, MonadReader env m, HasLogFunc env)
    => FilePath
    -> m (Either Text (Vector TCDentry))
loadFromFile mibPath = loadFromFileGen mibPath fileName


{-# INLINABLE getTCDMap #-}
getTCDMap :: Vector TCDentry -> HashMap Word32 TCDentry
getTCDMap vec = V.foldl' (\m e -> HM.insert (_tcdApid e) e m) HM.empty vec


