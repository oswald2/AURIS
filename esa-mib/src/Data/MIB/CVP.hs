{-# LANGUAGE
    TemplateHaskell
#-}
module Data.MIB.CVP
    ( CVPentry(..)
    , loadFromFile
    , getCVPMap
    , cvpTask
    , cvpType
    , cvpCvsID
    ) where

import           RIO
import qualified RIO.Vector                    as V
import           Control.Lens                   ( makeLenses )

import           Data.Text.Short                ( ShortText )

import           Data.Csv

import           Data.MIB.Load
import           Data.MIB.Types

import           Data.Multimap                 as M

data CVPentry = CVPentry
    { _cvpTask  :: !ShortText
    , _cvpType  :: CharDefaultTo "C"
    , _cvpCvsID :: !Int
    }
    deriving (Eq, Show)
makeLenses ''CVPentry


instance FromRecord CVPentry where
    parseRecord = genericParse (== 3) CVPentry


fileName :: FilePath
fileName = "cvp.dat"


loadFromFile
    :: (MonadIO m, MonadReader env m, HasLogFunc env, HasCallStack)
    => FilePath
    -> m (Either Text (Vector CVPentry))
loadFromFile mibPath = loadFromFileGen mibPath fileName


{-# INLINABLE getCVPMap #-}
getCVPMap :: Vector CVPentry -> Multimap ShortText CVPentry
getCVPMap vec = V.foldl' (\m e -> M.insert (_cvpTask e) e m) M.empty vec
