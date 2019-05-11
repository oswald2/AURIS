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


class HasConfig env where
    getConfig :: Getter env Config


class HasPUSState env where
    appStateG :: Getter env AppState

class (HasConfig env, HasPUSState env) => HasGlobalState env where
    raiseEvent :: env -> Event -> IO ()


instance HasConfig GlobalState where
    getConfig = to glsConfig

instance HasPUSState GlobalState where
    appStateG = to glsState

instance HasGlobalState GlobalState where
    raiseEvent state event = glsRaiseEvent state event
