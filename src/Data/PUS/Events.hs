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
    )
where

import GHC.Generics

import Data.Binary
import Data.Aeson
import RIO.Text (Text)


data Event = 
    EVIllegalTCFrame Text
    deriving (Eq, Show, Read, Generic)

instance Binary Event

instance ToJSON Event where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Event 
