{-# LANGUAGE OverloadedStrings
    , BangPatterns
    , NoImplicitPrelude
    , GADTs
    , DeriveGeneric
#-}
module Interface.Actions
    (
    Action(..)
    )
where

import           RIO

import           Data.Binary
import           Data.Aeson

import           Data.PUS.TCRequest


data Action where
    ActionQuit :: Action
    ActionSendTCRequest ::TCRequest -> Action
    deriving (Read, Show, Generic)

instance Binary Action
instance FromJSON Action
instance ToJSON Action



