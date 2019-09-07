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

import           Data.PUS.TMFrame
import           Data.PUS.TMFrameExtractor

import           Control.PUS.Classes

import           Protocol.NCTRS



ncduToTMFrameC
  :: (MonadIO m, MonadReader env m, HasGlobalState env)
  => ConduitT NcduTmDu TMFrame m ()
ncduToTMFrameC = ncduTmLoadC .| tmFrameDecodeC

ncduTmLoadC :: (MonadIO m) => ConduitT NcduTmDu ByteString m ()
ncduTmLoadC = awaitForever $ \ncdu -> do
  yield (ncdu ^. ncduTmData)

