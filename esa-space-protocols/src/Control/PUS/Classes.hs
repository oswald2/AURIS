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
    , RankNTypes
#-}
module Control.PUS.Classes
  ( HasConfig(..)
  , HasPUSState(..)
  , HasGlobalState(..)
  , HasFOPState(..)
  , HasCorrelationState(..)
  , HasMissionSpecific(..)
  )
where

import           RIO                     hiding ( to )
import qualified RIO.HashMap.Partial           as HM

import           Control.Lens.Getter

import           Data.PUS.Config
import           Data.PUS.Events
import           General.PUSTypes
import           Data.PUS.GlobalState
import           Data.PUS.MissionSpecific.Definitions
                                                ( PUSMissionSpecific )

-- | This class specifies how to get a configuration
class HasConfig env where
    getConfig :: Getter env Config

-- | Class for getting the application state
class HasPUSState env where
    appStateG :: Getter env AppState

-- | Class for getting the missions specific handlers
class HasMissionSpecific env where
    getMissionSpecific :: Getter env PUSMissionSpecific

-- | Class for getting the FOP1 State
class HasFOPState env where
    copStateG :: Getter env COP1State
    fopStateG :: VCID -> env -> FOP1State

class HasCorrelationState env where
    corrStateG :: Getter env CorrelationVar

-- | Class for accessing the global state
class (HasConfig env, 
    HasPUSState env, 
    HasFOPState env, 
    HasMissionSpecific env, 
    HasCorrelationState env) => HasGlobalState env where
    raiseEvent :: env -> Event -> IO ()



instance HasConfig GlobalState where
  getConfig = to glsConfig

instance HasPUSState GlobalState where
  appStateG = to glsState

instance HasMissionSpecific GlobalState where
  getMissionSpecific = to glsMissionSpecific

instance HasFOPState GlobalState where
  copStateG = to glsFOP1
  fopStateG vcid env = glsFOP1 env HM.! vcid

instance HasCorrelationState GlobalState where 
    corrStateG = to glsCorrState


instance HasGlobalState GlobalState where
  raiseEvent = glsRaiseEvent


