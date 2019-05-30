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

import           ByteString.StrictBuilder

import           Conduit
import           Data.Conduit.TQueue
import           Data.TimerWheel
import           Control.Lens                   ( makeLenses
                                                , (.~)
                                                , (+~)
                                                )

--import           UnliftIO.STM

import           Control.PUS.Classes

import           Data.PUS.Config
import           Data.PUS.COP1Types
import           Data.PUS.TCTransferFrame
--import           Data.PUS.CLCW
import           Data.PUS.Types          hiding ( Initial )
--import           Data.PUS.Time
import           Data.PUS.TCDirective
import           Data.PUS.Segment
import           Data.PUS.GlobalState
import           Data.PUS.Events

import           Protocol.Switcher
import           Protocol.ProtocolInterfaces


data FOPData = FOPData {
    _fvcid :: VCID
    , _fwaitQueue :: TMVar EncodedSegment
    , _fcop1Queue :: COP1Queues
    , _fout :: TBQueue TCTransferFrame
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

  initADWithoutCLCW :: (MonadIO m, MonadReader env m, HasGlobalState env) => FOPData -> State m Initial -> m (State m Active)
  initADWithCLCW :: (MonadIO m, MonadReader env m, HasGlobalState env) => FOPData -> State m Initial -> m (State m InitialisingWithoutBC)
  initADWithUnlock :: (MonadIO m, MonadReader env m, HasGlobalState env) => FOPData -> State m Initial -> m (State m InitialisingWithBC)
  initADWithSetVR :: (MonadIO m, MonadReader env m, HasGlobalState env) => FOPData -> Word8 -> State m Initial -> m (State m InitialisingWithBC)


newtype FOPMachineT env m a = FOPTMachineT { runFOPMachineT :: m a }
  deriving (Functor, Monad, Applicative, MonadIO)


data FOPMachineState s where
  Active :: FOPMachineState Active
  RetransmitWithoutWait :: FOPMachineState RetransmitWithoutWait
  RetransmitWithWait :: FOPMachineState RetransmitWithWait
  InitialisingWithoutBC :: FOPMachineState InitialisingWithoutBC
  InitialisingWithBC :: FOPMachineState InitialisingWithBC
  Initial :: FOPMachineState Initial


instance (MonadIO m, MonadReader env m, HasGlobalState env) => FOPMachine (FOPMachineT env m) where
    type State (FOPMachineT env m) = FOPMachineState

    initial = pure Initial

    initADWithoutCLCW fopData _ = do
        env <- ask
        let st = fopStateG (fopData ^. fvcid) env
        res <- atomically $ initializeAD st fopData
        case res of
            Nothing -> pure ()
            Just seg -> liftIO $ raiseEvent env $ EVCOP1 (EV_ADPurgedWaitQueue (fopData ^. fvcid) seg)
        pure Active

    initADWithCLCW fopData _ = do
        env <- ask
        let st = fopStateG (fopData ^. fvcid) env
        res <- atomically $ initializeAD st fopData
        case res of
            Nothing -> pure ()
            Just seg -> liftIO $ raiseEvent env $ EVCOP1 (EV_ADPurgedWaitQueue (fopData ^. fvcid) seg)
        pure InitialisingWithoutBC

    initADWithUnlock fopData _ = do
        st <- fopStateG (fopData ^. fvcid) <$> ask
        env <- ask
        let st = fopStateG (fopData ^. fvcid) env
        res <- atomically $ do
            r <- initializeAD st fopData
            modifyTVar' st (\state -> state & fopBCout .~ toFlag Ready False)
            pure r
        case res of
            Nothing -> pure ()
            Just seg -> liftIO $ raiseEvent env $
                EVCOP1 (EV_ADPurgedWaitQueue (fopData ^. fvcid) seg)
        pure InitialisingWithBC

    initADWithSetVR fopData vr _ = do
        st <- fopStateG (fopData ^. fvcid) <$> ask
        env <- ask
        let st = fopStateG (fopData ^. fvcid) env
        res <- atomically $ do
            r <- initializeAD st fopData
            modifyTVar' st (\state ->
                    state & fopBCout .~ toFlag Ready False
                    & fopVS .~ vr
                    & fopNNR .~ vr
                    )
            pure r
        case res of
            Nothing -> pure ()
            Just seg -> liftIO $ raiseEvent env $
                EVCOP1 (EV_ADPurgedWaitQueue (fopData ^. fvcid) seg)
        pure InitialisingWithBC

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
                Nothing  -> pure Nothing
                Just seg -> case seg ^. encSegFlag of
                    SegmentStandalone -> pure (Just seg)
                    SegmentLast       -> pure (Just seg)
                    -- in case we are in the middle of a transmission
                    -- of more segments, throw them all away.
                    _                 -> clearWaitQueue
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
    inp <- liftIO $ atomically $ readCOP1Queue (fopData ^. fvcid)
                                               (fopData ^. fcop1Queue)
    case inp of
        Nothing   -> stateInactive fopData st
        Just inp' -> do
            case inp' of
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
                        liftIO $ raiseEvent env $ EVCOP1
                            (EV_ADInitWaitingCLCW (fopData ^. fvcid))
                        stateInitialisingWithoutBC fopData newst
                    InitADWithUnlock Unlock -> do
                        -- check BC_Out
                        bcout <- checkReadyOut fopData fopBCout
                        if bcout
                            then do
                                -- initialise the AD mode
                                newst <- initADWithUnlock fopData st
                                -- send the 'Unlock' directive
                                sendBCFrame fopData Unlock
                                -- state transition
                                stateInitialisingWithBC fopData newst
                            else stateInactive fopData st
                    InitADWithSetVR dir@(SetVR vr) -> do
                        bcout <- checkReadyOut fopData fopBCout
                        if bcout
                            then do
                                -- initialise the AD mode
                                newst <- initADWithSetVR fopData vr st
                                -- send the 'SetVR' directive
                                sendBCFrame fopData dir
                                -- state transition
                                stateInitialisingWithBC fopData newst
                            else stateInactive fopData st
                    _ -> stateInactive fopData st
                COP1CLCW _clcw  -> stateInactive fopData st
                COP1TimeoutCLCW -> stateInactive fopData st
            pure ()


-- | send a BC Frame to the out channel (to the interface for encoding and sending).
-- Creates a new BC Frame with the given directive as content, encodes the directive,
-- fills out the frame values, increments the V(S) counter of the COP-1 state machine
-- and sends the frame out to the channel.
sendBCFrame
    :: (MonadIO m, MonadReader env m, HasGlobalState env)
    => FOPData
    -> TCDirective
    -> m ()
sendBCFrame fopData directive = do
    env <- ask
    let cfg          = env ^. getConfig
        vcid         = fopData ^. fvcid
        scid         = cfgSCID cfg
        fopState     = fopStateG vcid env
        encDirective = builderBytes $ directiveBuilder directive
    atomically $ do
        -- read the current FOP-1 state
        st <- readTVar fopState

        -- create a new BC transfer frame and fill out the values
        let frame = TCTransferFrame
                { _tcFrameVersion = 0
                , _tcFrameFlag    = FrameBC
                , _tcFrameSCID    = scid
                , _tcFrameVCID    = vcid
                , _tcFrameLength  = 0
                , _tcFrameSeq     = st ^. fopVS
                , _tcFrameData    = encDirective
                }
        -- create a new state which has V(S) incremented
            newst = st & fopVS +~ 1

        -- write the new state
        writeTVar fopState newst
        -- write the frame to the out queue
        writeTBQueue (fopData ^. fout) frame



checkReadyOut
    :: (MonadIO m, MonadReader env m, HasFOPState env)
    => FOPData
    -> Getting (Flag Ready) FOPState (Flag Ready)
    -> m Bool
checkReadyOut fopData lens = do
    env <- ask
    let fopState = fopStateG (fopData ^. fvcid) env
    fsta <- atomically $ readTVar fopState
    let out = fsta ^. lens

    pure (fromFlag Ready out)


stateActive fopData st = do
    pure ()


-- | S2
stateRetransmitWithoutWait fopData st = do
    pure ()


    -- | S3
stateRetransmitWithWait fopData st = do
    pure ()


    -- | S4
stateInitialisingWithoutBC
    :: (MonadIO m, MonadReader env m, HasGlobalState env)
    => FOPData
    -> State m InitialisingWithoutBC
    -> m ()
stateInitialisingWithoutBC fopData st = do
    env <- ask
    let st = fopStateG (fopData ^. fvcid) env
    fopState <- liftIO $ atomically $ readTVar st

    let timerWheel = fopData ^. ftimerWheel
        timeout    = fopState ^. fopT1Initial

    cancel <- liftIO
        $ register timeout (notifyTimeoutInitCLCW fopData) timerWheel
    pure ()


    -- | S5
stateInitialisingWithBC fopData st = do
    pure ()


readSegment :: (MonadIO m) => TMVar EncodedSegment -> m EncodedSegment
readSegment var = atomically $ takeTMVar var


readInput :: (MonadIO m) => COP1Queue -> m COP1Input
readInput chan = atomically $ readTBQueue chan


notifyTimeoutInitCLCW :: FOPData -> IO ()
notifyTimeoutInitCLCW fopData = atomically $ sendCOP1Queue
    (fopData ^. fvcid)
    (fopData ^. fcop1Queue)
    COP1TimeoutCLCW



-- | This is a conduit for the input. @chan is a 'TBQueue' which is wrapped in
-- a input conduit (STM Conduit). @segBuffer is a 'TMVar' used as the '''waitQueue'''
-- for the COP-1 protocol.
--
-- So basically, this conduit gets encoded segments wrapped in ProtocolPackets as
-- input and places them in the TMVar. This may block if the COP-1 machine is busy
-- or in a state where it doesn't accept commands, which is the expected wait queue
-- behaviour. The COP-1 state machine will read the segment from the TMVar when
-- it is ready and forward it to another channel for TC Transfer Frame, CLTU and
-- finally NCTRS or SLE encoding.
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
