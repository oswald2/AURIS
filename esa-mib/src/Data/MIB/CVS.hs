{-# LANGUAGE
    TemplateHaskell
#-}
module Data.MIB.CVS
    ( CVSentry(..)
    , loadFromFile
    , getCVSMap
    , cvsID
    , cvsType
    , cvsSource
    , cvsStart
    , cvsInterval
    , cvsSPID
    , cvsUncertainty
    ) where

import           RIO
import qualified RIO.Vector                    as V
import qualified RIO.HashMap                   as HM
import           Control.Lens                   ( makeLenses )

import           Data.Csv

import           Data.MIB.Load

data CVSentry = CVSentry
    { _cvsID          :: !Int
    , _cvsType        :: !Char
    , _cvsSource      :: !Char
    , _cvsStart       :: !Word
    , _cvsInterval    :: !Word
    , _cvsSPID        :: Maybe Word32
    , _cvsUncertainty :: Maybe Int
    }
    deriving Show
makeLenses ''CVSentry

instance Eq CVSentry where
    f1 == f2 = _cvsID f1 == _cvsID f2



instance FromRecord CVSentry where
    parseRecord v
        | V.length v == 7 = genericParse (const True) CVSentry v
        | V.length v == 6 =  genericParse (const True) CVSentry
        $    v
        V.++ V.singleton "-1"
        | otherwise = mzero

fileName :: FilePath
fileName = "cvs.dat"


loadFromFile
    :: (MonadIO m, MonadReader env m, HasLogFunc env, HasCallStack)
    => FilePath
    -> m (Either Text (Vector CVSentry))
loadFromFile mibPath = loadFromFileGen mibPath fileName


{-# INLINABLE getCVSMap #-}
getCVSMap :: Vector CVSentry -> HashMap Int CVSentry
getCVSMap vec = V.foldl' (\m e -> HM.insert (_cvsID e) e m) HM.empty vec
