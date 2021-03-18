{-# LANGUAGE
    TemplateHaskell
#-}
module Data.MIB.PRV
    ( PRVentry(..)
    , loadFromFile
    , prvNumbr
    , prvMinVal
    , prvMaxVal
    ) where

import           RIO
import           Control.Lens                   ( makeLenses )

import           Data.Text.Short                ( ShortText )
import           Data.Csv

import           Data.MIB.Load


data PRVentry = PRVentry
    { _prvNumbr  :: !ShortText
    , _prvMinVal :: !ShortText
    , _prvMaxVal :: !ShortText
    }
    deriving Show
makeLenses ''PRVentry

instance Eq PRVentry where
    f1 == f2 = _prvNumbr f1 == _prvNumbr f2



instance FromRecord PRVentry where
    parseRecord = genericParse (== 3) PRVentry


fileName :: FilePath
fileName = "prv.dat"


loadFromFile
    :: (MonadIO m, MonadReader env m, HasLogFunc env, HasCallStack)
    => FilePath
    -> m (Either Text (Vector PRVentry))
loadFromFile mibPath = loadFromFileGen mibPath fileName


