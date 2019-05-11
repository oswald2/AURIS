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

import Data.Conduit

import Data.PUS.TCTransferFrame
import Data.PUS.CLTU


tcFrameToCltuC :: (Monad m) => ConduitT EncodedTCFrame CLTU m ()
tcFrameToCltuC = awaitForever $ \frame ->
    pure $ cltuNew (frame ^. encTcFrameData)

