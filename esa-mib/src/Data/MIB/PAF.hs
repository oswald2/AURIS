{-# LANGUAGE
    TemplateHaskell
#-}
module Data.MIB.PAF
    ( PAFentry(..)
    , loadFromFile
    , getPAFMap
    , pafNumbr
    , pafDescr
    , pafRawFmt
    , pafNcurve
    ) where

import           RIO
import qualified RIO.HashMap                   as HM
import qualified RIO.Vector                    as V
import           Control.Lens                   ( makeLenses )

import           Data.Text.Short                ( ShortText )
import           Data.Csv

import           Data.MIB.Load
import           Data.MIB.Types

data PAFentry = PAFentry
    { _pafNumbr  :: !ShortText
    , _pafDescr  :: !ShortText
    , _pafRawFmt :: CharDefaultTo "U"
    , _pafNcurve :: Maybe Int
    }
    deriving Show
makeLenses ''PAFentry

instance Eq PAFentry where
    f1 == f2 = _pafNumbr f1 == _pafNumbr f2



instance FromRecord PAFentry where
    parseRecord = genericParse (== 4) PAFentry


fileName :: FilePath
fileName = "cca.dat"


loadFromFile
    :: (MonadIO m, MonadReader env m, HasLogFunc env, HasCallStack)
    => FilePath
    -> m (Either Text (Vector PAFentry))
loadFromFile mibPath = loadFromFileGen mibPath fileName


getPAFMap :: Vector PAFentry -> HashMap ShortText PAFentry
getPAFMap = V.foldl' (\m e -> HM.insert (_pafNumbr e) e m) HM.empty
