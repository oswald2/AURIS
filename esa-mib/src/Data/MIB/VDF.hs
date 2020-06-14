{-# LANGUAGE
    OverloadedStrings
    , BangPatterns
    , NoImplicitPrelude
    , DataKinds
#-}
module Data.MIB.VDF
  ( VDFentry(..)
  , loadFromFile
  )
where

import           RIO

import           Data.Text.Short                ( ShortText )
import           Data.Csv
import qualified RIO.Vector.Partial            as V
                                                ( last )

import           Data.MIB.Types
import           Data.MIB.Load


data VDFentry = VDFentry {
    _vdfName :: !ShortText
    , _vdfComment :: !ShortText
    , _vdfDomainID :: Maybe Int
    , _vdfRelease :: DefaultTo 0
    , _vdfIssue :: DefaultTo 0
} deriving (Eq, Show)




instance FromRecord VDFentry where
  parseRecord = genericParse (== 5) VDFentry


fileName :: FilePath
fileName = "vdf.dat"


loadFromFile
  :: (MonadIO m, MonadReader env m, HasLogFunc env, HasCallStack)
  => FilePath
  -> m (Either Text VDFentry)
loadFromFile mibPath = do
  v <- loadFromFileGen mibPath fileName
  case v of
    Left  err -> pure (Left err)
    Right vec -> pure (Right (V.last vec))

