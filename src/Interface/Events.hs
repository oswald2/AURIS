{-|
Module      : Interface.Events
Description : Contains all events that can be raised via this interface
Copyright   : (c) Michael Oswald, 2019
License     : BSD-3
Maintainer  : michael.oswald@onikudaki.net
Stability   : experimental
Portability : POSIX


This module contains all Events, that can be raised on this interface
-}
{-# LANGUAGE OverloadedStrings
    , BangPatterns
    , NoImplicitPrelude
    , DeriveGeneric
#-}
module Interface.Events
(
    IfEvent(..)
)
where



import RIO

import Data.Binary
import Data.Aeson

import Data.PUS.Events


-- | The general application based event. Contains also all PUS events from
-- 'Data.PUS.Events'
data IfEvent = 
    EventPUS Event
    deriving (Read, Show, Generic)


instance Binary IfEvent
instance FromJSON IfEvent
instance ToJSON IfEvent
