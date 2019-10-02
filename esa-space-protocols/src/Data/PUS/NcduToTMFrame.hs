{-# LANGUAGE 
    OverloadedStrings
    , BangPatterns
    , NoImplicitPrelude

#-}
module Data.PUS.NcduToTMFrame
  ( ncduToTMFrameC
  , ncduTmLoadC
  )
where


import           RIO


import           Conduit

import           Data.PUS.TMFrameExtractor
import           Data.PUS.TMStoreFrame
import           Data.PUS.EncTime

import           Control.PUS.Classes

import           Protocol.NCTRS



ncduToTMFrameC
  :: (MonadIO m, MonadReader env m, HasGlobalState env)
  => ConduitT NcduTmDu TMStoreFrame m ()
ncduToTMFrameC = ncduTmLoadC .| tmFrameDecodeC

ncduTmLoadC :: (MonadIO m) => ConduitT NcduTmDu (CDSTime, ByteString) m ()
ncduTmLoadC = awaitForever $ \ncdu -> do
  yield (ncdu ^. ncduTmHeader . ncduTmERT, ncdu ^. ncduTmData)

