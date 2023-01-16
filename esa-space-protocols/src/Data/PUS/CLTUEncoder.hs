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
    ( cltuToNcduC
    ) where

import           RIO

import           Data.Conduit

import           Protocol.NCTRS

import           Control.PUS.Classes

import           Data.PUS.CLTU
import           Data.PUS.TCRequest
import           Data.PUS.Verification

import           General.PUSTypes
import           General.Time

import           Text.Show.Pretty



-- | This is just very basic now, it directly converts the CLTU
-- into a NCDU with just default values. There is much to be done
-- here
cltuToNcduC
    :: (MonadIO m, MonadReader env m, HasLogFunc env, HasVerif env)
    => ConduitT EncodedCLTU NcduTcDu m ()
cltuToNcduC = awaitForever $ \(EncodedCLTU cltu rqst) -> do
    let hdr     = NcduTcHeader 0 NCDU_TC_CLTU_TYPE (mkSCID 0)
        rqstID  = rqst ^. tcReqRequestID
        cltuhdr = NcduTcCltuHeader
            BD
            (getRqstID rqstID)
            (rqst ^. tcReqVCID)
            (fromMaybe (mkMAPID 0) (tcReqMapID rqst))
            0
            0
            0
            0
            0
            0
        dat  = NcduTcDuCltuData cltuhdr cltu
        ncdu = NcduTcDu hdr dat
    logDebug $ "NCDU: " <> fromString (ppShow ncdu)
    now <- liftIO getCurrentTime
    yield ncdu
    -- also set to RELEASED
    env <- ask
    liftIO $ requestReleased env rqstID now StRSuccess
