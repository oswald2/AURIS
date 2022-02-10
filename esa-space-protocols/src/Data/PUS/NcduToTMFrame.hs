{-# LANGUAGE 
    OverloadedStrings
    , BangPatterns
    , NoImplicitPrelude

#-}
module Data.PUS.NcduToTMFrame
    ( ncduToTMFrameC
    , ncduTmLoadC
    ) where


import           RIO


import           Conduit

import           General.Types

import           Data.PUS.EncTime
import           Data.PUS.TMFrameExtractor
import           Data.PUS.TMStoreFrame

import           Control.PUS.Classes

import           Protocol.NCTRS
import           Protocol.ProtocolInterfaces


ncduToTMFrameC
    :: (MonadIO m, MonadReader env m, HasGlobalState env)
    => ProtocolInterface
    -> ConduitT NcduTmDu TMStoreFrame m ()
ncduToTMFrameC pIf = ncduTmLoadC pIf .| tmFrameDecodeC

ncduTmLoadC
    :: (MonadIO m)
    => ProtocolInterface
    -> ConduitT NcduTmDu (CDSTime, ProtocolInterface, ByteString) m ()
ncduTmLoadC pIf = awaitForever $ \ncdu -> do
    yield (ncdu ^. ncduTmHeader . ncduTmERT, pIf, toBS (ncdu ^. ncduTmData))

