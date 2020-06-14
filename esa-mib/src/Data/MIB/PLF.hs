{-# LANGUAGE
    TemplateHaskell
#-}
module Data.MIB.PLF
  ( PLFentry(..)
  , loadFromFile
  , plfName
  , plfSPID
  , plfOffBy
  , plfOffBi
  , plfNbOcc
  , plfLgOcc
  , plfTime
  , plfTdOcc
  )
where

import           RIO
import           Control.Lens                   ( makeLenses )

import           Data.Text.Short                ( ShortText )
import           Data.Csv


import           Data.MIB.Load
import           Data.MIB.Types
--import           General.PUSTypes



data PLFentry = PLFentry {
    _plfName :: !ShortText,
    _plfSPID :: !Word32,
    _plfOffBy :: !Int,
    _plfOffBi :: !Int,
    _plfNbOcc :: Maybe Int,
    _plfLgOcc :: Maybe Int,
    _plfTime :: DefaultTo 0,
    _plfTdOcc :: Maybe Int
} deriving (Show, Read)
makeLenses ''PLFentry

instance Eq PLFentry where
  plf1 == plf2 = _plfName plf1 == _plfName plf2


instance Ord PLFentry where
  compare plf1 plf2 =
    let val1 = _plfOffBy plf1 * 8 + _plfOffBi plf1
        val2 = _plfOffBy plf2 * 8 + _plfOffBi plf2
    in  compare val1 val2



instance FromRecord PLFentry where
  parseRecord = genericParse (>= 8) PLFentry


fileName :: FilePath
fileName = "plf.dat"

loadFromFile
  :: (MonadIO m, MonadReader env m, HasLogFunc env, HasCallStack)
  => FilePath
  -> m (Either Text (Vector PLFentry))
loadFromFile mibPath = loadFromFileGen mibPath fileName


