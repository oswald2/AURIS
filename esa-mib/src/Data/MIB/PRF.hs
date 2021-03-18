{-# LANGUAGE
    TemplateHaskell
#-}
module Data.MIB.PRF
    ( PRFentry(..)
    , loadFromFile
    , getPRFMap
    , prfNumbr
    , prfDescr
    , prfInter
    , prfDispFormat
    , prfRadix
    , prfNRange
    , prfUnit
    ) where

import           RIO
import qualified RIO.HashMap                   as HM
import qualified RIO.Vector                    as V
import           Control.Lens                   ( makeLenses )

import           Data.Text.Short                ( ShortText )
import           Data.Csv

import           Data.MIB.Load
import           Data.MIB.Types

data PRFentry = PRFentry
    { _prfNumbr      :: !ShortText
    , _prfDescr      :: !ShortText
    , _prfInter      :: CharDefaultTo "R"
    , _prfDispFormat :: CharDefaultTo "U"
    , _prfRadix      :: CharDefaultTo "D"
    , _prfNRange     :: Maybe Int
    , _prfUnit       :: !ShortText
    }
    deriving Show
makeLenses ''PRFentry

instance Eq PRFentry where
    f1 == f2 = _prfNumbr f1 == _prfNumbr f2



instance FromRecord PRFentry where
    parseRecord = genericParse (== 7) PRFentry


fileName :: FilePath
fileName = "prf.dat"


loadFromFile
    :: (MonadIO m, MonadReader env m, HasLogFunc env, HasCallStack)
    => FilePath
    -> m (Either Text (Vector PRFentry))
loadFromFile mibPath = loadFromFileGen mibPath fileName


getPRFMap :: Vector PRFentry -> HashMap ShortText PRFentry
getPRFMap = V.foldl' (\m e -> HM.insert (_prfNumbr e) e m) HM.empty
