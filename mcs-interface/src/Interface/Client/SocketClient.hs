{-# LANGUAGE OverloadedStrings
    , BangPatterns
    , NoImplicitPrelude
#-}
module Interface.Client.SocketClient
(   
    callAction
    , eventProcessor
)
where


import RIO

import Conduit 
--import UnliftIO.STM

import Interface.Actions
import Interface.Events



callAction :: (MonadIO m) => TBQueue Action -> Action -> m ()
callAction queue action = atomically $ writeTBQueue queue action


eventProcessor :: ConduitT IfEvent Void m ()
eventProcessor = undefined