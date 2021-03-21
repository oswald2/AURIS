{-# LANGUAGE
    TemplateHaskell
#-}
module Data.MIB.CCA
    ( CCAentry(..)
    , loadFromFile
    , getCCAMap
    , ccaNumbr 
    , ccaDescr 
    , ccaEngFmt
    , ccaRawFmt
    , ccaRadix 
    , ccaUnit  
    , ccaNcurve
    ) where

import           RIO
import qualified RIO.HashMap                   as HM
import qualified RIO.Vector                    as V
import           Control.Lens                   ( makeLenses )

import           Data.Text.Short                ( ShortText )
import           Data.Csv

import           Data.MIB.Load
import           Data.MIB.Types

data CCAentry = CCAentry
    { _ccaNumbr  :: !ShortText
    , _ccaDescr  :: !ShortText
    , _ccaEngFmt :: CharDefaultTo "R"
    , _ccaRawFmt :: CharDefaultTo "U"
    , _ccaRadix  :: CharDefaultTo "D"
    , _ccaUnit   :: !ShortText
    , _ccaNcurve :: Maybe Int
    }
    deriving Show
makeLenses ''CCAentry

instance Eq CCAentry where
    f1 == f2 = _ccaNumbr f1 == _ccaNumbr f2



instance FromRecord CCAentry where
    parseRecord = genericParse (== 7) CCAentry


fileName :: FilePath
fileName = "cca.dat"


loadFromFile
    :: (MonadIO m, MonadReader env m, HasLogFunc env, HasCallStack)
    => FilePath
    -> m (Either Text (Vector CCAentry))
loadFromFile mibPath = loadFromFileGen mibPath fileName


getCCAMap :: Vector CCAentry -> HashMap ShortText CCAentry
getCCAMap = V.foldl' (\m e -> HM.insert (_ccaNumbr e) e m) HM.empty
