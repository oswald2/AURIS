{-# LANGUAGE OverloadedStrings
    , BangPatterns
    , GeneralizedNewtypeDeriving
    , DeriveGeneric
    , RecordWildCards
    , NoImplicitPrelude
#-}
module Data.PUS.CLTUEncoder
    (
        cltuToNcduC
    )
where

import RIO

import Data.Conduit

import Protocol.NCTRS
import Data.PUS.CLTU
import Data.PUS.Types


-- | This is just very basic now, it directly converts the CLTU
-- into a NCDU with just default values. There is much to be done
-- here
cltuToNcduC :: (MonadIO m, MonadReader env m, HasLogFunc env) =>
    ConduitT EncodedCLTU NcduTcDu m ()
cltuToNcduC = awaitForever $ \(EncodedCLTU cltu) -> do
    let hdr = NcduTcHeader 0 NCDU_TC_CLTU_TYPE (mkSCID 0)
        cltuhdr = NcduTcCltuHeader BD 0 (mkVCID 0) (mkMAPID 0) 0 0 0 0 0 0
        dat = NcduTcDuCltuData cltuhdr cltu
        ncdu = NcduTcDu hdr dat
    logDebug $ "NCDU: " <> displayShow ncdu
    yield ncdu