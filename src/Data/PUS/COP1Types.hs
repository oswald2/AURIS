{-# LANGUAGE OverloadedStrings
    , BangPatterns
    , GeneralizedNewtypeDeriving
    , DeriveGeneric
    , RecordWildCards
    , NoImplicitPrelude
    , BinaryLiterals
    , FlexibleInstances
    , GADTs
    , ExistentialQuantification
    , TemplateHaskell
#-}
module Data.PUS.COP1Types
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

    , COP1Directive(..)
    , COP1Input(..)
    , COP1Queue
    )
where


import           RIO

import           Control.Lens                   ( makeLenses )

import           Data.PUS.Types
import           Data.PUS.Segment
import           Data.PUS.Time
import           Data.PUS.TCDirective
import           Data.PUS.CLCW


data TTType = TTAlert | TTSuspend
    deriving (Eq, Ord, Enum, Show, Read)




data FOPState = FOPState {
_fopWaitFlag :: !Bool,
_fopLockoutFlag :: !Bool,
_fopRetransmitFlag :: !Bool,
_fopVS :: !Word8,
_fopWaitQueue :: ![EncodedSegment],
_fopSentQueue :: ![EncodedSegment],
_fopToBeRetransmitted :: !Bool,
_fopADout :: Flag Ready,
_fopBDout :: Flag Ready,
_fopBCout :: Flag Ready,
_fopNNR :: !Word8,
_fopT1Initial :: TimeSpan,
_fopTimeoutType :: !TTType,
_fopTransmissionLimit :: !Word8,
_fopTransmissionCount :: !Word8,
_fopSuspendState :: !Int,
_fopSlidingWinWidth :: !Word8
} deriving (Show, Read)
makeLenses ''FOPState


initialFOPState :: FOPState
initialFOPState = FOPState
    { _fopWaitFlag          = False
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
    , _fopT1Initial         = toTimeSpan $ mkTimeSpan Seconds 5
    , _fopTimeoutType       = TTAlert
    , _fopTransmissionLimit = 5
    , _fopTransmissionCount = 0
    , _fopSuspendState      = 0
    , _fopSlidingWinWidth   = 10
    }



data COP1Directive =
    InitADWithoutCLCW
    | InitADWithCLCW
    | InitADWithUnlock TCDirective
    | InitADWithSetVR  TCDirective
    | TerminateAD
    | ResumeAD
    | SetVS !Word8
    | SetFOPSlidingWindowWidth !Word8
    | SetT1Initial TimeSpan
    | SetTransmissionLimit !Word8
    | SetTimeoutType TTType


data COP1Input =
    COP1Dir COP1Directive
    | COP1CLCW CLCW



type COP1Queue = TBQueue COP1Input