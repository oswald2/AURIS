{-# LANGUAGE OverloadedStrings
    , TemplateHaskell
    , NoImplicitPrelude
    , BangPatterns
#-}
module Data.PUS.PUSState
  ( PUSState(..)
  , defaultPUSState
  , newState
  , nextADCnt
  , pusStEpoch
  , pusStLeapSeconds
  )
where

import           RIO

import           Control.Lens                   ( makeLenses )

import           General.Time

import           Data.PUS.Config


-- | This is the internal state of the PUS library.
data PUSState = PUSState {
    _pusStADCounter :: !Word8
    , _pusStEpoch :: !Epoch
    , _pusStLeapSeconds :: LeapSeconds
}

makeLenses ''PUSState



newState :: Monad m => Epoch -> LeapSeconds -> m PUSState
newState epoch leaps = do
  let state = PUSState { _pusStADCounter   = 0
                       , _pusStEpoch       = epoch
                       , _pusStLeapSeconds = leaps
                       }
  pure state


defaultPUSState :: Monad m => Config -> m PUSState
defaultPUSState cfg = do
  let epoch = getEpoch (cfgEpoch cfg) (cfgLeapSeconds cfg)
      state = PUSState { _pusStADCounter   = 0
                       , _pusStEpoch       = epoch
                       , _pusStLeapSeconds = cfgLeapSeconds cfg
                       }
  pure state



nextADCnt :: PUSState -> (PUSState, Word8)
nextADCnt st =
  let cnt    = st ^. pusStADCounter
      !newSt = over pusStADCounter (+ 1) st
  in  (newSt, cnt)
