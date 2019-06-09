{-# LANGUAGE OverloadedStrings
    , BangPatterns
    , NoImplicitPrelude
#-}
module Interface.Client.SocketClient
(   

)
where


import RIO

import Conduit 
import Data.Conduit.Network

import Interface.Actions
import Interface.Events




reconnectClient :: MonadUnliftIO m => ClientSettings -> (AppData -> m a) -> m a
reconnectClient settings = undefined


callAction :: TBQueue Action -> Action -> m ()
callAction = undefined


eventProcessor :: ConduitT IfEvent Void m ()
eventProcessor = undefined