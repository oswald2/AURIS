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

executeAction :: (MonadIO m) => Interface -> Action -> m ()
executeAction interface ActionQuit = 
    liftIO $ callInterface interface actionQuit
executeAction interface (ActionSendTCRequest rqst) = 
    liftIO $ callInterface interface actionSendTCRequest rqst