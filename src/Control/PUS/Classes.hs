{-|
Module      : Control.PUS.Classes
Description : Various classes for config- and state-handling within RIO
Copyright   : (c) Michael Oswald, 2019
License     : BSD-3
Maintainer  : michael.oswald@onikudaki.net
Stability   : experimental
Portability : POSIX

This module contains classes with the HasX pattern to have finer gained 
access control to IO functions. Used within the encoding conduits.
|-}
{-# LANGUAGE
    NoImplicitPrelude
    , MultiParamTypeClasses
    , FlexibleInstances
#-}
module Control.PUS.Classes
    ( HasConfig(..)
    , HasPUSState(..)
    , HasGlobalState(..)
    -- , getConfig
    -- , getGlobalState
    -- , getPUSState
    -- , withPUSState
    -- , withPUSState_
    -- , nextADCount
    -- , raiseEvent
    )
where

import           RIO                     hiding ( to
                                                , view
                                                )

import           Control.Lens.Getter

import           Data.PUS.Config
import           Data.PUS.Events
import           Data.PUS.GlobalState

-- | This class specifies how to get a configuration
class HasConfig env where
    getConfig :: Getter env Config

-- | Class for getting the application state
class HasPUSState env where
    appStateG :: Getter env AppState

-- | Class for accessing the global state
class (HasConfig env, HasPUSState env) => HasGlobalState env where
    raiseEvent :: env -> Event -> IO ()


instance HasConfig GlobalState where
    getConfig = to glsConfig

instance HasPUSState GlobalState where
    appStateG = to glsState

instance HasGlobalState GlobalState where
    raiseEvent state event = glsRaiseEvent state event


