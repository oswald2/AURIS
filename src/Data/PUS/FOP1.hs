{-# LANGUAGE GeneralizedNewtypeDeriving
    , BangPatterns
    , NoImplicitPrelude
    , TemplateHaskell
    , OverloadedStrings
    , GADTs
    , TypeFamilies
    , ConstrainedClassMethods
#-}
module Data.PUS.FOP1
    ( cop1Conduit
    , fop1Program
    )
where

import           RIO
import qualified RIO.Seq                       as S

import           Conduit
import           Data.Conduit.TQueue
import           Data.TimerWheel
import           Control.Lens                   ( makeLenses
                                                , (.~)
                                                )

--import           UnliftIO.STM

import           Control.PUS.Classes

import           Data.PUS.COP1Types
--import           Data.PUS.TCTransferFrame
--import           Data.PUS.CLCW
import           Data.PUS.Types
--import           Data.PUS.Time
--import           Data.PUS.TCDirective
import           Data.PUS.Segment
import           Data.PUS.GlobalState
import           Data.PUS.Events

import           Protocol.Switcher
import           Protocol.ProtocolInterfaces


data FOPData = FOPData {
    _fvcid :: VCID
    , _fwaitQueue :: TMVar EncodedSegment
    , _fcop1Queue :: COP1Queue
    , _ftimerWheel :: TimerWheel
    }
makeLenses ''FOPData



_checkSlidingWinWidth :: Word8 -> Bool
_checkSlidingWinWidth w = (2 < w) && (w < 254) && even w




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

  initADWithoutCLCW :: (MonadIO m, MonadReader env m, HasFOPState env) => FOPData -> State m Initial -> m (State m Active)
  initADWithCLCW :: (MonadIO m, MonadReader env m, HasFOPState env) => FOPData -> State m Initial -> m (State m InitialisingWithoutBC)
  initADWithUnlock :: State m Initial -> m (State m InitialisingWithBC)
  initADWithSetVR :: State m Initial -> m (State m InitialisingWithBC)


newtype FOPMachineT env m a = FOPTMachineT { runFOPMachineT :: m a }
  deriving (Functor, Monad, Applicative, MonadIO)


data FOPMachineState s where
  Active :: FOPMachineState Active
  RetransmitWithoutWait :: FOPMachineState RetransmitWithoutWait
  RetransmitWithWait :: FOPMachineState RetransmitWithWait
  InitialisingWithoutBC :: FOPMachineState InitialisingWithoutBC
  InitialisingWithBC :: FOPMachineState InitialisingWithBC
  Initial :: FOPMachineState Initial


instance (MonadIO m, MonadReader env m, HasFOPState env) => FOPMachine (FOPMachineT env m) where
    type State (FOPMachineT env m) = FOPMachineState

    initial = pure Initial

    initADWithoutCLCW fopData _ = do
        st <- fopStateG (fopData ^. fvcid) <$> ask
        atomically $ initializeAD st fopData
        pure Active

    initADWithCLCW fopData _ = do
        st <- fopStateG (fopData ^. fvcid) <$> ask
        atomically $ initializeAD st fopData
        pure InitialisingWithoutBC


-- | initializes the AD mode. In case the wait queue is purged, the segment
-- is returned
initializeAD :: FOP1State -> FOPData -> STM (Maybe EncodedSegment)
initializeAD st fopData = do
    modifyTVar'
        st
        (\state ->
            state
                &  fopWaitFlag
                .~ False
                &  fopLockoutFlag
                .~ False
                &  fopRetransmitFlag
                .~ False
                &  fopSentQueue
                .~ S.empty
                &  fopToBeRetransmitted
                .~ False
                &  fopADout
                .~ toFlag Ready True
                &  fopBDout
                .~ toFlag Ready True
                &  fopBCout
                .~ toFlag Ready True
                &  fopTransmissionCount
                .~ 0
        )
    -- also purge the wait queue
    let clearWaitQueue = do
            tc <- tryTakeTMVar (fopData ^. fwaitQueue)
            case tc of
                Nothing -> pure Nothing
                Just seg ->
                    case seg ^. encSegFlag of
                        SegmentStandalone -> pure (Just seg)
                        SegmentLast -> pure (Just seg)
                        -- in case we are in the middle of a transmission
                        -- of more segments, throw them all away.
                        _ -> clearWaitQueue
    clearWaitQueue


-- | this is the whole state machine of the FOP-1 starting point
fop1Program
    :: (MonadIO m, MonadReader env m, HasGlobalState env, FOPMachine m)
    => FOPData
    -> m ()
fop1Program fopData = do
    st <- initial
    stateInactive fopData st


stateInactive
    :: (MonadIO m, MonadReader env m, HasGlobalState env, FOPMachine m)
    => FOPData
    -> State m Initial
    -> m ()
stateInactive fopData st = do
    inp <- liftIO $ readInput (fopData ^. fcop1Queue)
    case inp of
        COP1Dir dir -> case dir of
            InitADWithoutCLCW -> do
                newst <- initADWithoutCLCW fopData st
                env   <- ask
                liftIO $ raiseEvent env $ EVCOP1
                    (EV_ADInitializedWithoutCLCW (fopData ^. fvcid))
                -- go to next state
                stateActive fopData newst
            InitADWithCLCW -> do
                newst <- initADWithCLCW fopData st
                env   <- ask
                liftIO $ raiseEvent env $ EVCOP1 (EV_ADInitWaitingCLCW (fopData ^. fvcid))
                stateInitialisingWithoutBC fopData newst
                pure ()
            _ -> pure ()
        COP1CLCW clcw -> pure ()
    pure ()


stateActive fopData st = do
    pure ()


-- | S2
stateRetransmitWithoutWait fopData st = do
    pure ()


    -- | S3
stateRetransmitWithWait fopData st = do
    pure ()


    -- | S4
stateInitialisingWithoutBC fopData st = do
    pure ()


    -- | S5
stateInitialisingWithBC fopData st = do
    pure ()


readSegment :: (MonadIO m) => TMVar EncodedSegment -> m EncodedSegment
readSegment var = atomically $ takeTMVar var


readInput :: (MonadIO m) => COP1Queue -> m COP1Input
readInput chan = atomically $ readTBQueue chan



cop1Conduit
    :: (MonadIO m)
    => NCTRSChan EncodedSegment
    -> TMVar EncodedSegment
    -> ConduitT (ProtocolPacket EncodedSegment) () m ()
cop1Conduit chan segBuffer = sourceTBQueue chan .| proc
  where
    proc :: (MonadIO m) => ConduitT (ProtocolPacket EncodedSegment) () m ()
    proc = do
        x <- await
        case x of
            Nothing  -> pure ()
            Just pkt -> do
                atomically $ putTMVar segBuffer (pkt ^. protContent)
                cop1Conduit chan segBuffer
