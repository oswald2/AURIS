{-# LANGUAGE TemplateHaskell #-}
module Data.MIB.CDF
    ( CDFentry(..)
    , loadFromFile
    , cdfName
    , cdfElemType
    , cdfDescr
    , cdfElemLen
    , cdfBit
    , cdfGrpSize
    , cdfPName
    , cdfInter
    , cdfValue
    , cdfTmID
    ) where

import           RIO
import           Control.Lens                   ( makeLenses )

import           Data.Text.Short                ( ShortText )
import           Data.Csv

import           Data.MIB.Load
import           Data.MIB.Types




data CDFentry = CDFentry
    { _cdfName     :: !ShortText
    , _cdfElemType :: !Char
    , _cdfDescr    :: !ShortText
    , _cdfElemLen  :: !Int
    , _cdfBit      :: !Int
    , _cdfGrpSize  :: DefaultTo 0
    , _cdfPName    :: !ShortText
    , _cdfInter    :: CharDefaultTo "R"
    , _cdfValue    :: !ShortText
    , _cdfTmID     :: !ShortText
    }
    deriving (Show, Read)
makeLenses ''CDFentry



instance Eq CDFentry where
    c1 == c2 = c1 ^. cdfName == c2 ^. cdfName

instance FromRecord CDFentry where
    parseRecord = genericParse (>= 10) CDFentry


fileName :: FilePath
fileName = "cdf.dat"

loadFromFile
    :: (MonadIO m, MonadReader env m, HasLogFunc env, HasCallStack)
    => FilePath
    -> m (Either Text (Vector CDFentry))
loadFromFile mibPath = loadFromFileGen mibPath fileName


