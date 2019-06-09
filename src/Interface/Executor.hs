{-|
Module      : Interface.Executor
Description : Executes the actions from the 'Interface.Actions' module
Copyright   : (c) Michael Oswald, 2019
License     : BSD-3
Maintainer  : michael.oswald@onikudaki.net
Stability   : experimental
Portability : POSIX

This module provides a function for executing the passed 'Action' on the
local interface
-}
{-# LANGUAGE OverloadedStrings
    , BangPatterns
    , NoImplicitPrelude
#-}
module Interface.Executor
    ( executeAction
    )
where

import           RIO

import           Interface.Interface
import           Interface.Actions



-- | Executes the action. This just takes the action from the 
-- 'Action' type and calls the local interface with the provided data
executeAction :: (MonadIO m) => Interface -> Action -> m ()
executeAction interface ActionQuit = 
    liftIO $ callInterface interface actionQuit
executeAction interface (ActionSendTCRequest rqst) = 
    liftIO $ callInterface interface actionSendTCRequest rqst