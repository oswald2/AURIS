{-|
Module      : Data.PUS.Events
Description : This file represents the Events which can be raised
              by the library
Copyright   : (c) Michael Oswald, 2019
License     : BSD-3
Maintainer  : michael.oswald@onikudaki.net
Stability   : experimental
Portability : POSIX

Contains just the Events, which can be raised by the library
|-}
{-# LANGUAGE
    OverloadedStrings
    , DeriveGeneric
#-}
module Data.PUS.Events
    (
        Event(..)
        , EventArea(..)
    )
where

import GHC.Generics

import Data.Binary
import Data.Aeson
import RIO.Text (Text)

-- | An event area. An application can register on certain event Areas in
-- order to not get flooded with all kinds of events
data EventArea =
    EVACommanding
    | EVAAlarms
    | EVATelemetry
    deriving (Eq, Ord, Enum, Show, Read, Generic)

instance Binary EventArea
instance FromJSON EventArea
instance ToJSON EventArea where
    toEncoding = genericToEncoding defaultOptions

-- | The events themselves
data Event =
      EV_IllegalTCFrame EventArea Text
    | EV_NCDUParseError EventArea Text
    deriving (Eq, Show, Read, Generic)

instance Binary Event

instance ToJSON Event where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Event
