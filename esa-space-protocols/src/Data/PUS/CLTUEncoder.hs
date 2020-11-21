{-|
Module      : Data.PUS.CLTUEncoder
Description : Provides a conduit for translating a CLTU into a NCDU 
Copyright   : (c) Michael Oswald, 2019
License     : BSD-3
Maintainer  : michael.oswald@onikudaki.net
Stability   : experimental
Portability : POSIX

Provides a conduit for translating a CLTU into a NCDU for transmission via NCTRS protocol
|-}
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
import General.PUSTypes


-- | This is just very basic now, it directly converts the CLTU
-- into a NCDU with just default values. There is much to be done
-- here
cltuToNcduC :: (MonadIO m, MonadReader env m, HasLogFunc env) =>
    ConduitT EncodedCLTU NcduTcDu m ()
cltuToNcduC = awaitForever $ \(EncodedCLTU cltu _) -> do
    let hdr = NcduTcHeader 0 NCDU_TC_CLTU_TYPE (mkSCID 0)
        cltuhdr = NcduTcCltuHeader BD 0 (mkVCID 0) (mkMAPID 0) 0 0 0 0 0 0
        dat = NcduTcDuCltuData cltuhdr cltu
        ncdu = NcduTcDu hdr dat
    logDebug $ "NCDU: " <> displayShow ncdu
    yield ncdu