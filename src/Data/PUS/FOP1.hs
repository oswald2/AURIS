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

withFOPState_
    :: (MonadIO m, MonadReader env m, HasFOPState env)
    => FOPData
    -> (FOPState -> STM FOPState)
    -> m ()
withFOPState_ fopData stmAction = do
    env <- ask
    let fopState = fopStateG (fopData ^. fvcid) env
    atomically $ do
        st    <- readTVar fopState

        newst <- stmAction st

        writeTVar fopState newst


withFOPState
    :: (MonadIO m, MonadReader env m, HasFOPState env)
    => FOPData
    -> (FOPState -> STM (FOPState, a))
    -> m a
withFOPState fopData stmAction = do
    env <- ask
    let fopState = fopStateG (fopData ^. fvcid) env
    atomically $ do
        st              <- readTVar fopState

        (newst, result) <- stmAction st

        writeTVar fopState newst
        pure result

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


data InitWithBCStateOut m =
    IWBCInitial (State m Initial)
    | IWBCInitialisingWithBC (State m InitialisingWithBC)


data ResumeOut m =
    RSInitial (State m Initial)
    | RSActive (State m Active)
    | RSRetransmitWOWait (State m RetransmitWithoutWait)
    | RSRetransmitWWait (State m RetransmitWithWait)
    | RSInitialisingWOBC (State m InitialisingWithoutBC)

class FOPMachine m where
  type State m :: * -> *
  initial :: m (State m Initial)

  initADWithoutCLCW :: (MonadIO m, MonadReader env m, HasGlobalState env) =>
    FOPData -> State m Initial -> m (State m Active)
  initADWithCLCW :: (MonadIO m, MonadReader env m, HasGlobalState env) =>
    FOPData -> State m Initial -> m (IO Bool, State m InitialisingWithoutBC)
  initADWithUnlock :: (MonadIO m, MonadReader env m, HasGlobalState env) =>
    FOPData -> State m Initial -> m (InitWithBCStateOut m)
  initADWithSetVR :: (MonadIO m, MonadReader env m, HasGlobalState env) =>
    FOPData -> TCDirective -> State m Initial -> m (InitWithBCStateOut m)

  resumeAD :: (MonadIO m, MonadReader env m, HasGlobalState env) =>
    FOPData -> State m Initial -> m (IO Bool, ResumeOut m)

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
        join $ withFOPState fopData $ \state -> do
            let newst = initializeState state
            purged <- purgeWaitQueue fopData
            let action = liftIO $ do
                    when purged $ raiseEvent env $ EVCOP1 (EV_ADPurgedWaitQueue (fopData ^. fvcid))
                    liftIO $ raiseEvent env $ EVCOP1 (EV_ADInitializedWithoutCLCW (fopData ^. fvcid))
            pure (newst, action)
        pure Active

    initADWithCLCW fopData _ = do
        env <- ask
        let st = fopStateG (fopData ^. fvcid) env
        cancelTimer <- join $ withFOPState fopData $ \state -> do
            let newst = initializeState state
            purged <- purgeWaitQueue fopData

            let timeout = newst ^. fopT1Initial
                timerWheel = fopData ^. ftimerWheel
                action = liftIO $ do
                    when purged $ raiseEvent env $ EVCOP1 (EV_ADPurgedWaitQueue (fopData ^. fvcid))
                    raiseEvent env $ EVCOP1 (EV_ADInitWaitingCLCW (fopData ^. fvcid))
                    register timeout (notifyTimeout fopData) timerWheel
            pure (newst, action)
        pure (cancelTimer, InitialisingWithoutBC)

    initADWithUnlock fopData _ = do
        env <- ask
        let st = fopStateG (fopData ^. fvcid) env
        -- perform actions on the state. withFOPState returns an IO action which
        -- is executed with join
        join $ withFOPState fopData $ \state ->
            -- only to this if BC out is ready
            if toBool (state ^. fopBCout)
                then do
                    -- intialize the state to AD
                    let newst = initializeState state
                    -- purge the wait queue
                    purged <- purgeWaitQueue fopData
                    -- encode the Unlock directive and send it downstream
                    newst1 <- sendBCFrameSTM (env ^. getConfig) fopData newst Unlock
                    -- determine a new state, where BC out is set to true since when
                    -- sendBCFrameSTM aborts, it will be retried
                    let newst2 = newst1 & fopBCout .~ toFlag Ready True
                        -- action execute after return from STM call
                        action = liftIO $ do
                            when purged $ raiseEvent env $ EVCOP1 (EV_ADPurgedWaitQueue (fopData ^. fvcid))
                            pure (IWBCInitialisingWithBC InitialisingWithBC)
                    pure (newst2, action)
                -- in case BC out was not ready, just remain in the initial state
                else pure (state, pure (IWBCInitial Initial))

    -- | initialises the AD mode with only SetVR. Other directives are ignored.
    initADWithSetVR fopData setVr@(SetVR vr) _ = do
        env <- ask
        let st = fopStateG (fopData ^. fvcid) env
        -- perform actions on the state. withFOPState returns an IO action which
        -- is executed with join
        join $ withFOPState fopData $ \state ->
            -- only to this if BC out is ready
            if toBool (state ^. fopBCout)
                then do
                    -- intialize the state to AD
                    let newst = initializeState state & fopVS .~ vr & fopNNR .~ vr
                    -- purge the wait queue
                    purged <- purgeWaitQueue fopData
                    -- encode the Unlock directive and send it downstream
                    newst1 <- sendBCFrameSTM (env ^. getConfig) fopData newst setVr
                    -- determine a new state, where BC out is set to true since when
                    -- sendBCFrameSTM aborts, it will be retried
                    let newst2 = newst1 & fopBCout .~ toFlag Ready True
                        -- action execute after return from STM call
                        action = liftIO $ do
                            when purged $ raiseEvent env $ EVCOP1 (EV_ADPurgedWaitQueue (fopData ^. fvcid))
                            pure (IWBCInitialisingWithBC InitialisingWithBC)
                    pure (newst2, action)
                -- in case BC out was not ready, just remain in the initial state
                else pure (state, pure (IWBCInitial Initial))
    initADWithSetVR _ _ _ = pure (IWBCInitial Initial)

    -- | resume AD mode operation when it was suspended.
    resumeAD fopData _ = do
        (action, nextState) <- withFOPState fopData $ \state ->
            -- check the suspend state
            case state ^. fopSuspendState of
                1 -> resume state (RSActive Active)
                2 -> resume state (RSRetransmitWOWait RetransmitWithoutWait)
                3 -> resume state (RSRetransmitWWait RetransmitWithWait)
                4 -> resume state (RSInitialisingWOBC InitialisingWithoutBC)
                _ -> pure (state, (pure (pure True), RSInitial Initial))

        -- execute the returned action
        cancelTimer <- liftIO action
        -- return the result
        pure (cancelTimer, nextState)
        where
            -- performs the actual resume
            resume state nextSt = do
                let timeOut = state ^. fopT1Initial
                    -- initialise the suspend state back to 0
                    newst = state & fopSuspendState .~ 0
                    timerWheel = fopData ^. ftimerWheel
                    -- return the action to set the timer
                    action = register timeOut (notifyTimeout fopData) timerWheel
                pure (newst, (action, nextSt))


-- | initialise the state
initializeState :: FOPState -> FOPState
initializeState state =
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

-- | increment the V(S) counter in the state
incrementVS :: FOPState -> FOPState
incrementVS state = state & fopVS +~ 1

-- | purges the wait queue in case of initialisation of AD mode
purgeWaitQueue :: FOPData -> STM Bool
purgeWaitQueue fopData = do
    tc <- tryTakeTMVar (fopData ^. fwaitQueue)
    case tc of
        Nothing  -> pure False
        Just seg -> case seg ^. encSegFlag of
            SegmentStandalone -> pure True
            SegmentLast       -> pure True
            -- in case we are in the middle of a transmission
            -- of more segments, throw them all away.
            _                 -> do
                let loop = do
                        seg <- takeTMVar (fopData ^. fwaitQueue)
                        case seg ^. encSegFlag of
                            SegmentStandalone -> do
                                putTMVar (fopData ^. fwaitQueue) seg
                                pure True
                            SegmentFirst -> do
                                putTMVar (fopData ^. fwaitQueue) seg
                                pure True
                            _ -> loop
                loop


-- | this is the whole state machine of the FOP-1 starting point
fop1Program
    :: (MonadIO m, MonadReader env m, HasGlobalState env, FOPMachine m)
    => FOPData
    -> m ()
fop1Program fopData = do
    st <- initial
    stateInactive fopData (pure True) st


stateInactive
    :: (MonadIO m, MonadReader env m, HasGlobalState env, FOPMachine m)
    => FOPData
    -> IO Bool
    -> State m Initial
    -> m ()
stateInactive fopData cancelTimer st = do
    env <- ask
    inp <- liftIO $ atomically $ readCOP1Queue (fopData ^. fvcid)
                                               (fopData ^. fcop1Queue)
    case inp of
        Nothing   -> stateInactive fopData cancelTimer st
        Just inp' -> do
            case inp' of
                COP1Dir dir -> case dir of
                    InitADWithoutCLCW ->
                        initADWithoutCLCW fopData st
                            >>= stateActive fopData cancelTimer
                    InitADWithCLCW -> initADWithCLCW fopData st
                        >>= \(ct, newst) -> stateInitialisingWithoutBC fopData ct newst
                    InitADWithUnlock Unlock -> do
                        newst <- initADWithUnlock fopData st
                        case newst of
                            IWBCInitial sta ->
                                stateInactive fopData cancelTimer sta
                            IWBCInitialisingWithBC sta ->
                                stateInitialisingWithBC fopData sta
                    InitADWithSetVR setVR -> do
                        newst <- initADWithSetVR fopData setVR st
                        case newst of
                            IWBCInitial sta ->
                                stateInactive fopData cancelTimer sta
                            IWBCInitialisingWithBC sta ->
                                stateInitialisingWithBC fopData sta
                    TerminateAD -> stateInactive fopData cancelTimer st
                    ResumeAD    -> do
                        (cancelTimer', newst) <- resumeAD fopData st
                        case newst of
                            RSInitial st ->
                                stateInactive fopData cancelTimer' st
                            RSActive st -> stateActive fopData cancelTimer' st
                            RSRetransmitWOWait st -> stateRetransmitWithoutWait
                                fopData
                                cancelTimer'
                                st
                            RSRetransmitWWait st ->
                                stateRetransmitWithWait fopData cancelTimer' st
                            RSInitialisingWOBC st -> stateInitialisingWithoutBC
                                fopData
                                cancelTimer'
                                st
                    SetVS vs -> do
                        join $ withFOPState fopData $ \state -> do
                            let newst = state & fopVS .~ vs & fopNNR .~ vs
                                action = liftIO $ raiseEvent env $ EVCOP1 (EV_ADConfirmSetVS (fopData ^. fvcid) vs)
                            pure (newst, action)
                        stateInactive fopData cancelTimer st
                    _ -> stateInactive fopData cancelTimer st
                COP1CLCW _clcw -> stateInactive fopData cancelTimer st
                COP1Timeout    -> stateInactive fopData cancelTimer st

            pure ()



sendBCFrameSTM :: Config -> FOPData -> FOPState -> TCDirective -> STM FOPState
sendBCFrameSTM cfg fopData fopState directive = do
    let vcid         = fopData ^. fvcid
        scid         = cfgSCID cfg
        encDirective = builderBytes $ directiveBuilder directive
-- create a new BC transfer frame and fill out the values
        frame        = TCTransferFrame
            { _tcFrameVersion = 0
            , _tcFrameFlag    = FrameBC
            , _tcFrameSCID    = scid
            , _tcFrameVCID    = vcid
            , _tcFrameLength  = 0
            , _tcFrameSeq     = fopState ^. fopVS
            , _tcFrameData    = encDirective
            }
-- create a new state which has V(S) incremented
        newst = fopState & fopVS +~ 1

    -- write the frame to the out queue
    writeTBQueue (fopData ^. fout) frame

    pure newst


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





stateActive fopData cancelTimer st = do
    pure ()


-- | S2
stateRetransmitWithoutWait fopData cancelTimer st = do
    pure ()


    -- | S3
stateRetransmitWithWait fopData cancelTimer st = do
    pure ()


    -- | S4
stateInitialisingWithoutBC
    :: (MonadIO m, MonadReader env m, HasGlobalState env)
    => FOPData
    -> IO Bool
    -> State m InitialisingWithoutBC
    -> m ()
stateInitialisingWithoutBC fopData cancelTimer st = do
    pure ()


    -- | S5
stateInitialisingWithBC fopData st = do
    pure ()


readSegment :: (MonadIO m) => TMVar EncodedSegment -> m EncodedSegment
readSegment var = atomically $ takeTMVar var


readInput :: (MonadIO m) => COP1Queue -> m COP1Input
readInput chan = atomically $ readTBQueue chan


notifyTimeout :: FOPData -> IO ()
notifyTimeout fopData = atomically
    $ sendCOP1Queue (fopData ^. fvcid) (fopData ^. fcop1Queue) COP1Timeout



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
