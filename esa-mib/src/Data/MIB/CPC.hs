{-# LANGUAGE TemplateHaskell #-}
module Data.MIB.CPC
    ( CPCentry(..)
    , loadFromFile
    , getCPCMap
    , cpcName
    , cpcDescr
    , cpcPTC
    , cpcPFC
    , cpcDispFmt
    , cpcRadix
    , cpcUnit
    , cpcCateg
    , cpcPrfRef
    , cpcCcaRef
    , cpcPafRef
    , cpcInter
    , cpcDefVal
    , cpcCorr
    , cpcObtID
    ) where

import           Control.Lens                   ( makeLenses )
import           RIO
import qualified RIO.HashMap                   as HM
import qualified RIO.Vector                    as V

import           Data.Csv
import           Data.Text.Short                ( ShortText )

import           Data.MIB.Load
import           Data.MIB.Types




data CPCentry = CPCentry
    { _cpcName    :: !ShortText
    , _cpcDescr   :: !ShortText
    , _cpcPTC     :: !Int
    , _cpcPFC     :: !Int
    , _cpcDispFmt :: CharDefaultTo "R"
    , _cpcRadix   :: CharDefaultTo "D"
    , _cpcUnit    :: !ShortText
    , _cpcCateg   :: CharDefaultTo "N"
    , _cpcPrfRef  :: !ShortText
    , _cpcCcaRef  :: !ShortText
    , _cpcPafRef  :: !ShortText
    , _cpcInter   :: CharDefaultTo "R"
    , _cpcDefVal  :: !ShortText
    , _cpcCorr    :: CharDefaultTo "Y"
    , _cpcObtID   :: DefaultTo 0
    }
    deriving (Show, Read)
makeLenses ''CPCentry



instance Eq CPCentry where
    c1 == c2 = c1 ^. cpcName == c2 ^. cpcName

instance FromRecord CPCentry where
    parseRecord v
        | V.length v > 15 = genericParse (const True) CPCentry (V.slice 0 15 v)
        | V.length v == 15 = genericParse (const True) CPCentry v
        | V.length v == 13 = genericParse (const True)
                                          CPCentry
                                          (v V.++ V.fromList ["Y", "0"])
        | otherwise = mzero

fileName :: FilePath
fileName = "cpc.dat"

loadFromFile
    :: (MonadIO m, MonadReader env m, HasLogFunc env, HasCallStack)
    => FilePath
    -> m (Either Text (Vector CPCentry))
loadFromFile mibPath = loadFromFileGen mibPath fileName


getCPCMap :: Vector CPCentry -> HashMap ShortText CPCentry
getCPCMap = V.foldl' (\m e -> HM.insert (_cpcName e) e m) HM.empty
