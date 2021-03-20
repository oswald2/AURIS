{-# LANGUAGE
    TemplateHaskell
#-}
module Data.MIB.CVE
    ( CVEentry(..)
    , loadFromFile
    , getCVEMap
    , cveID      
    , cveParName 
    , cveInter   
    , cveVal     
    , cveTol     
    , cveCheck   
    ) where

import           RIO
import qualified RIO.Vector                    as V
import qualified RIO.HashMap                   as HM
import           Control.Lens                   ( makeLenses )

import           Data.Text.Short                ( ShortText )
import           Data.Csv

import           Data.MIB.Load
import           Data.MIB.Types

data CVEentry = CVEentry
    { _cveID      :: !Int
    , _cveParName :: !ShortText
    , _cveInter   :: CharDefaultTo "R"
    , _cveVal     :: !ShortText
    , _cveTol     :: !ShortText
    , _cveCheck   :: CharDefaultTo "B"
    }
    deriving Show
makeLenses ''CVEentry

instance Eq CVEentry where
    f1 == f2 = _cveID f1 == _cveID f2



instance FromRecord CVEentry where
    parseRecord = genericParse (== 6) CVEentry


fileName :: FilePath
fileName = "cve.dat"


loadFromFile
    :: (MonadIO m, MonadReader env m, HasLogFunc env, HasCallStack)
    => FilePath
    -> m (Either Text (Vector CVEentry))
loadFromFile mibPath = loadFromFileGen mibPath fileName


{-# INLINABLE getCVEMap #-}
getCVEMap :: Vector CVEentry -> HashMap Int CVEentry
getCVEMap vec = V.foldl' (\m e -> HM.insert (_cveID e) e m) HM.empty vec
