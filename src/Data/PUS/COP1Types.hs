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
    , State(..)
    , TTType(..)
    , initialFOPState
    , fopVCID
    , fopWaitFlag
    , fopLockoutFlag
    , fopRetransmitFlag
    , fopVS
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
    , COP1Queues
    , sendCOP1Queue
    , readCOP1Queue
    , sendCOP1Q
    , readCOP1Q
    )
where


import           RIO
import           RIO.Seq
import qualified RIO.HashMap                   as HM

import           Control.Lens                   ( makeLenses )

import           Data.Fixed
import           Data.Binary
import           Data.Aeson

import           Data.PUS.Types
import           Data.PUS.TCDirective
import           Data.PUS.CLCW
import           Data.PUS.TCFrameTypes




-- | Timeout Type. If a timeout occurs, it can either be an alert (error condition)
-- or it can suspend the AD mode. In this case, the AD mode can be resumed via a
-- directive
data TTType = TTAlert | TTSuspend
    deriving (Eq, Ord, Enum, Show, Read, Generic)

instance Binary TTType
instance FromJSON TTType
instance ToJSON TTType where
    toEncoding = genericToEncoding defaultOptions



data State =
    -- | S1
    Active
    -- | S2
    | RetransmitWithoutWait
    -- | S3
    | RetransmitWithWait
    -- | S4
    | InitialisingWithoutBC
    -- | S5
    | InitialisingWithBC
    -- | S6
    | Initial
    deriving (Eq, Ord, Enum, Show, Read, Generic)

instance Binary State
instance FromJSON State
instance ToJSON State


-- | State of a FOP-1 machine. This state is local to a virtual channel
data FOPState = FOPState {
    _fopVCID :: !VCID,
    _fopWaitFlag :: !Bool,
    _fopLockoutFlag :: !Bool,
    _fopRetransmitFlag :: !Bool,
    _fopVS :: !Word8,
    _fopSentQueue :: Seq (TCFrameTransport, Bool),
    _fopToBeRetransmitted :: !Bool,
    _fopADout :: Flag Ready,
    _fopBDout :: Flag Ready,
    _fopBCout :: Flag Ready,
    _fopNNR :: !Word8,
    _fopT1Initial :: Fixed E6,
    _fopTimeoutType :: !TTType,
    _fopTransmissionLimit :: !Word8,
    _fopTransmissionCount :: !Word8,
    _fopSuspendState :: State,
    _fopSlidingWinWidth :: !Word8
} deriving (Show, Read)
makeLenses ''FOPState

-- | the initial State of the FOP-1 machine
initialFOPState :: VCID -> FOPState
initialFOPState vcid = FOPState
    { _fopVCID              = vcid
    , _fopWaitFlag          = False
    , _fopLockoutFlag       = False
    , _fopRetransmitFlag    = False
    , _fopVS                = 0
    , _fopSentQueue         = empty
    , _fopToBeRetransmitted = False
    , _fopADout             = toFlag Ready True
    , _fopBDout             = toFlag Ready True
    , _fopBCout             = toFlag Ready True
    , _fopNNR               = 0
    , _fopT1Initial         = 5
    , _fopTimeoutType       = TTAlert
    , _fopTransmissionLimit = 5
    , _fopTransmissionCount = 0
    , _fopSuspendState      = Initial
    , _fopSlidingWinWidth   = 10
    }


-- | All possible COP1 directives
data COP1Directive =
    InitADWithoutCLCW
    | InitADWithCLCW
    | InitADWithUnlock TCDirective
    | InitADWithSetVR  TCDirective
    | TerminateAD
    | ResumeAD
    | SetVS !Word8
    | SetFOPSlidingWindowWidth !Word8
    | SetT1Initial (Fixed E6)
    | SetTransmissionLimit !Word8
    | SetTimeoutType TTType

-- | Input to a COP-1 queue. This queue is used to notify the
-- state machine about certain conditions.
data COP1Input =
    -- | A COP-1 directive is sent to the FOP-1 machine
    COP1Dir COP1Directive
    -- | A CLCW has arrived. This is normally triggered by the telemtry chain
    | COP1CLCW CLCW
    -- | A timeout was triggered, while waiting for a CLCW to initialise the AD mode
    | COP1Timeout


-- | A single COP-1 queue. Note that we have multiple queues,
-- one for each virtual channel
type COP1Queue = TBQueue COP1Input

-- | All COP-1 queues. The virtual channel determines which
-- queue will be used
type COP1Queues = HashMap VCID COP1Queue


-- | Send a COP-1 message
sendCOP1Queue :: VCID -> COP1Queues -> COP1Input -> STM ()
sendCOP1Queue vcid queues inp = case HM.lookup vcid queues of
    Nothing    -> pure ()
    Just queue -> writeTBQueue queue inp

-- | Read a COP-1 message from the queue
readCOP1Queue :: VCID -> COP1Queues -> STM (Maybe COP1Input)
readCOP1Queue vcid queues = do
    case HM.lookup vcid queues of
        Nothing    -> pure Nothing
        Just queue -> Just <$> readTBQueue queue

-- | Read a COP-1 message from the queue
readCOP1Q :: COP1Queue -> STM COP1Input
readCOP1Q = readTBQueue

-- | Send a COP-1 message
sendCOP1Q :: COP1Queue -> COP1Input -> STM ()
sendCOP1Q = writeTBQueue
