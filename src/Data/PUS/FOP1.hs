{-# LANGUAGE GeneralizedNewtypeDeriving
    , BangPatterns
    , NoImplicitPrelude
    , TemplateHaskell
    , OverloadedStrings
    , GADTs
    , TypeFamilies
#-}
module Data.PUS.FOP1
  ( FOPState
  , initialFOPState
  , fopWaitFlag
  , fopLockoutFlag
  , fopRetransmitFlag
  , fopVS
  , fopWaitQueue
  , fopSentQueue
  , fopToBeRetransmitted
  , fopADout
  , fopBDout
  , fopBCout
  , fopNNR
  , fopT1Initial
  , fopTimeoutType
  , fopTransmissionLimit
  , fopTransmissionCount
  , fopSuspendState
  , fopSlidingWinWidth
  )
where

import           RIO

import           Control.Lens                   ( makeLenses )
--import           Control.Lens.Setter

--import qualified Data.ByteString.Lazy          as B

import           Data.PUS.TCTransferFrame
--import           Data.PUS.CLCW
import           Data.PUS.Types
import           Data.PUS.Time
import           Data.PUS.TCDirective
import           Data.PUS.Segment



data TTType = TTAlert | TTSuspend
  deriving (Eq, Ord, Enum, Show, Read)

data FOPState = FOPState {
  _fopWaitFlag :: !Bool,
  _fopLockoutFlag :: !Bool,
  _fopRetransmitFlag :: !Bool,
  _fopVS :: !Word8,
  _fopWaitQueue :: ![EncodedTCFrame],
  _fopSentQueue :: ![EncodedTCFrame],
  _fopToBeRetransmitted :: !Bool,
  _fopADout :: Flag Ready,
  _fopBDout :: Flag Ready,
  _fopBCout :: Flag Ready,
  _fopNNR :: !Word8,
  _fopT1Initial :: TimeSpan,
  _fopTimeoutType :: !TTType,
  _fopTransmissionLimit :: !Int,
  _fopTransmissionCount :: !Int,
  _fopSuspendState :: !Int,
  _fopSlidingWinWidth :: !Word8
  } deriving (Show, Read)

makeLenses ''FOPState


initialFOPState :: FOPState
initialFOPState = FOPState { _fopWaitFlag          = False
                           , _fopLockoutFlag       = False
                           , _fopRetransmitFlag    = False
                           , _fopVS                = 0
                           , _fopWaitQueue         = []
                           , _fopSentQueue         = []
                           , _fopToBeRetransmitted = False
                           , _fopADout             = toFlag Ready True
                           , _fopBDout             = toFlag Ready True
                           , _fopBCout             = toFlag Ready True
                           , _fopNNR               = 0
                           , _fopT1Initial = toTimeSpan $ mkTimeSpan Seconds 5
                           , _fopTimeoutType       = TTAlert
                           , _fopTransmissionLimit = 5
                           , _fopTransmissionCount = 0
                           , _fopSuspendState      = 0
                           , _fopSlidingWinWidth   = 10
                           }


_checkSlidingWinWidth :: Word8 -> Bool
_checkSlidingWinWidth w = (2 < w) && (w < 254) && even w


data COP1Directive =
  InitADWithoutCLCW
  | InitADWithCLCW
  | InitADWithUnlock TCDirective
  | InitADWithSetVR  TCDirective


data COP1Input = COP1Segment EncodedSegment


-- | S1
data Active
-- | S2
data RetransmitWithoutWait
-- | S3
data RetransmitWithWait
-- | S4
data InitialisingWithoutBC
-- | S5
data InitialisingWithBC
-- | S6
data Initial



class FOPMachine m where
  type State m :: * -> *
  initial :: m (State m Initial)

  initADWithoutCLCW :: State m Initial -> m (State m Active)
  initADWithCLCW :: State m Initial -> m (State m InitialisingWithoutBC)
  initADWithUnlock :: State m Initial -> m (State m InitialisingWithBC)
  initADWithSetVR :: State m Initial -> m (State m InitialisingWithBC)


--initialize :: (FOPMachine m, MonadGlobalState m) =>

