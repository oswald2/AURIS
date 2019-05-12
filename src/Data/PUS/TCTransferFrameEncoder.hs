{-# LANGUAGE OverloadedStrings
    , BangPatterns
    , GeneralizedNewtypeDeriving
    , DeriveGeneric
    , RecordWildCards
#-}
module Data.PUS.TCTransferFrameEncoder
    (
        tcFrameToCltuC
    )
where


import RIO

import Conduit

import Data.PUS.TCTransferFrame
import Data.PUS.CLTU


tcFrameToCltuC :: (MonadIO m, MonadReader env m, HasLogFunc env) => ConduitT EncodedTCFrame CLTU m ()
tcFrameToCltuC = awaitForever $ \frame -> do
    let new = cltuNew (frame ^. encTcFrameData)
    logDebug $ "New CLTU: " <> displayShow new
    yield new

