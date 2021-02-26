{-# LANGUAGE TemplateHaskell 
#-}
module Data.PUS.LiveState
    ( LiveStateState(..)
    , LiveState(..)
    , defaultLiveState
    , liStCommanding
    , liStTelemetry
    , liStEvents
    ) where

import           RIO
import           Control.Lens                   ( makeLenses )

data LiveStateState =
  Live | Stopped
  deriving (Eq, Ord, Enum, Show)


data LiveState = LiveState
    { _liStCommanding :: LiveStateState
    , _liStTelemetry  :: LiveStateState
    , _liStEvents     :: LiveStateState
    }
makeLenses ''LiveState


defaultLiveState :: LiveState
defaultLiveState = LiveState Live Live Live