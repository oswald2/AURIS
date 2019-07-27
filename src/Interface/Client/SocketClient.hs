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

import Interface.Actions
import Interface.Events



callAction :: TBQueue Action -> Action -> m ()
callAction = undefined


eventProcessor :: ConduitT IfEvent Void m ()
eventProcessor = undefined