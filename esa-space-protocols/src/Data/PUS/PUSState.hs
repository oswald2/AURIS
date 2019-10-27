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
    , pusStCorrelation
    , CorrelationType(..)
    )
where

import           RIO

import           Control.Lens                   ( makeLenses )

import           General.Time

import           Data.PUS.Config



data CorrelationType =
  CorrNo
  | CorrSYS
  | CorrERT
  | CorrPKT
  deriving (Eq, Ord, Enum, Show, Generic)


-- | This is the internal state of the PUS library.
data PUSState = PUSState {
    _pusStADCounter :: !Word8
    , _pusStEpoch :: !Epoch
    , _pusStLeapSeconds :: LeapSeconds
    , _pusStCorrelation :: CorrelationType
  } deriving (Show, Generic)
makeLenses ''PUSState



newState :: Monad m => Epoch -> LeapSeconds -> m PUSState
newState epoch leaps = do
    let state = PUSState { _pusStADCounter   = 0
                         , _pusStEpoch       = epoch
                         , _pusStLeapSeconds = leaps
                         , _pusStCorrelation = CorrNo
                         }
    pure state


defaultPUSState :: Monad m => Config -> m PUSState
defaultPUSState cfg = do
    let epoch = getEpoch (cfgEpoch cfg) (cfgLeapSeconds cfg)
        state = PUSState { _pusStADCounter   = 0
                         , _pusStEpoch       = epoch
                         , _pusStLeapSeconds = cfgLeapSeconds cfg
                         , _pusStCorrelation = CorrNo
                         }
    pure state



nextADCnt :: PUSState -> (PUSState, Word8)
nextADCnt st =
    let cnt    = st ^. pusStADCounter
        !newSt = over pusStADCounter (+ 1) st
    in  (newSt, cnt)
