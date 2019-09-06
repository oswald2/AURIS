{-|
Module      : Interface.Actions
Description : All actions which are callable from external interfaces
Copyright   : (c) Michael Oswald, 2019
License     : BSD-3
Maintainer  : michael.oswald@onikudaki.net
Stability   : experimental
Portability : POSIX

This module contains the central data type of actions of the interface.
Each constructor is an action that corresponds to an action in the 'ActionTable'
in the 'Interface'. The actions are all serializable in the current ways:

 * as Haskell Strings via Read and Show instances
 * as Binary via 'Data.Binary' instances
 * as JSON via 'Data.Aeson' instances

-}
{-# LANGUAGE OverloadedStrings
    , BangPatterns
    , NoImplicitPrelude
    , GADTs
    , DeriveGeneric
#-}
module Interface.Actions
    ( Action(..)
    )
where

import           RIO

import           Data.Binary
import           Data.Aeson
import           Codec.Serialise

import           Data.PUS.TCRequest


-- | The central 'Action' type. Contains all actions which should be
-- callable from external clients (like GUIs, scripts, command line etc).
-- For each action defined in the 'ActionTable' of the 'Interface' there
-- should be one constructor in this data type.

-- The action is then executed in the 'Executor' module
data Action where
    ActionQuit ::Action
    ActionSendTCRequest ::TCRequest -> Action
    deriving (Read, Show, Generic)

instance Binary Action
instance Serialise Action
instance FromJSON Action
instance ToJSON Action



