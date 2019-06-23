{-# LANGUAGE GeneralizedNewtypeDeriving
    , BangPatterns
    , NoImplicitPrelude
    , TemplateHaskell
    , OverloadedStrings
    , GADTs
    , TypeFamilies
    , ConstrainedClassMethods
    , ScopedTypeVariables
    , RankNTypes
    , ExistentialQuantification
    , MultiWayIf
#-}
module Data.PUS.FOP1
    ( cop1Conduit
    , startFOP1
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
import           Data.PUS.TCFrameTypes
import           Data.PUS.CLCW
import           Data.PUS.Types
--import           Data.PUS.Time
import           Data.PUS.TCDirective
import           Data.PUS.Segment
--import           Data.PUS.GlobalState
import           Data.PUS.Events
import           Data.PUS.TCRequest

import           Protocol.Switcher
import           Protocol.ProtocolInterfaces


data FOPData = FOPData {
    _fvcid :: VCID
    , _fwaitQueue :: TMVar EncodedSegment
    , _fcop1Queue :: COP1Queue
    , _fout :: TBQueue TCFrameTransport
    , _fstartTimer :: forall a. FOPState -> FOPData -> IO a -> STM (FOPState, IO (a, IO Bool))
    }
makeLenses ''FOPData


startFOP1
    :: (MonadIO m, MonadReader env m, HasGlobalState env)
    => VCID
    -> COP1Queue
    -> TMVar EncodedSegment
    -> TBQueue TCFrameTransport
    -> m ()
startFOP1 vcid cop1Queue waitQueue outQueue = do
    timerWheel <- liftIO $ new 5 0.1
    let fopData =
            FOPData vcid waitQueue cop1Queue outQueue (startTimerSTM timerWheel)
    fop1Program fopData
    pure ()

startTimerSTM
    :: TimerWheel
    -> FOPState
    -> FOPData
    -> IO a
    -> STM (FOPState, IO (a, IO Bool))
startTimerSTM timerWheel state fopData action = do
    let timeOut = state ^. fopT1Initial
-- initialise the suspend state back to 0
        newst   = state & fopSuspendState .~ Initial
-- return the action to set the timer
        act     = liftIO $ do
            result      <- action
            cancelTimer <- register timeOut (notifyTimeout fopData) timerWheel
            pure (result, cancelTimer)
    pure (newst, act)



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



initADWithoutCLCW
    :: (MonadIO m, MonadReader env m, HasGlobalState env) => FOPData -> m ()
initADWithoutCLCW fopData = do
    env <- ask
    join $ withFOPState fopData $ \state -> do
        let newst = initializeState state
        purged <- purgeWaitQueue fopData
        let action = liftIO $ do
                when purged $ raiseEvent env $ EVCOP1
                    (EVADPurgedWaitQueue (fopData ^. fvcid))
                liftIO $ raiseEvent env $ EVCOP1
                    (EVADInitializedWithoutCLCW (fopData ^. fvcid))
        pure (newst, action)
    pure ()


initADWithCLCW
    :: (MonadIO m, MonadReader env m, HasGlobalState env)
    => FOPData
    -> m (IO Bool)
initADWithCLCW fopData = do
    env <- ask
    act <- withFOPState fopData $ \state -> do
        let newst = initializeState state
        purged <- purgeWaitQueue fopData

        let action = do
                when purged $ raiseEvent env $ EVCOP1
                    (EVADPurgedWaitQueue (fopData ^. fvcid))
                raiseEvent env $ EVCOP1 (EVADInitWaitingCLCW (fopData ^. fvcid))

        (fopData ^. fstartTimer) newst fopData action
    (_, cancelTimer) <- liftIO act
    pure cancelTimer

initADWithUnlock
    :: (MonadIO m, MonadReader env m, HasGlobalState env)
    => FOPData
    -> m (State, IO Bool)
initADWithUnlock fopData = do
    env <- ask
    -- perform actions on the state. withFOPState returns an IO action which
    -- is executed afterwards
    act <- withFOPState fopData $ \state ->
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
                    when purged $ raiseEvent env $ EVCOP1
                        (EVADPurgedWaitQueue (fopData ^. fvcid))
                    pure InitialisingWithBC
            (fopData ^. fstartTimer) newst2 fopData action
-- in case BC out was not ready, just remain in the initial state
        else pure (state, pure (Initial, pure True))
    liftIO act

-- | initialises the AD mode with only SetVR. Other directives are ignored.
initADWithSetVR
    :: (MonadIO m, MonadReader env m, HasGlobalState env)
    => FOPData
    -> TCDirective
    -> m State
initADWithSetVR fopData setVr@(SetVR vr) = do
    env <- ask
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
                    when purged $ raiseEvent env $ EVCOP1
                        (EVADPurgedWaitQueue (fopData ^. fvcid))
                    pure InitialisingWithBC
            pure (newst2, action)
    -- in case BC out was not ready, just remain in the initial state
        else pure (state, pure Initial)
initADWithSetVR _ _ = pure Initial

    -- | resume AD mode operation when it was suspended.
resumeAD
    :: (MonadIO m, MonadReader env m, HasGlobalState env)
    => FOPData
    -> m (State, IO Bool)
resumeAD fopData = do
    action <- withFOPState fopData $ \state ->
        -- check the suspend state
                                               case state ^. fopSuspendState of
        Active                -> resume state Active
        RetransmitWithoutWait -> resume state RetransmitWithoutWait
        RetransmitWithWait    -> resume state RetransmitWithWait
        InitialisingWithoutBC -> resume state InitialisingWithoutBC
        InitialisingWithBC    -> resume state InitialisingWithBC
        _                     -> pure (state, pure (Initial, pure True))

    -- execute the returned action, also raise event that we have resumed
    env                      <- ask
    (nextState, cancelTimer) <- liftIO $ do
        res@(nextState, _) <- action
        raiseEvent env (EVCOP1 (EVResumedAD (fopData ^. fvcid) nextState))
        pure res

    -- return the result
    pure (nextState, cancelTimer)
  where
        -- performs the actual resume
    resume :: FOPState -> a -> STM (FOPState, IO (a, IO Bool))
    resume state nextSt = do
        let -- initialise the suspend state back to 0
            newst = state & fopSuspendState .~ Initial
        (fopData ^. fstartTimer) newst fopData (pure nextSt)


processResume
    :: -- (MonadIO m, MonadReader env m, HasGlobalState env)
    env
    -> VCID
    -> IO Bool
    -> FOPData
    -> FOPState
    -> StateActions env m
    -> STM (FOPState, m ())
processResume env vcid cancelTimer fopData fops stateActions = do
    case fops ^. fopSuspendState of
        Active -> e31 stateActions env vcid cancelTimer fopData fops
        RetransmitWithoutWait ->
            e32 stateActions env vcid cancelTimer fopData fops
        RetransmitWithWait ->
            e33 stateActions env vcid cancelTimer fopData fops
        InitialisingWithoutBC ->
            e34 stateActions env vcid cancelTimer fopData fops
        _ -> e30 stateActions env vcid cancelTimer fopData fops


resumeADState :: (MonadIO m, HasGlobalState env) =>
    (FOPData -> IO Bool -> m ())
    -> State
    -> env
    -> VCID
    -> IO Bool
    -> FOPData
    -> FOPState
    -> STM (FOPState, m ())
resumeADState nextState state env _vcid _cancelTimer fopData fops = do
    (newst, action) <- (fopData ^. fstartTimer) fops fopData
        (raiseEvent env (EVCOP1 (EVResumedAD (fopData ^. fvcid) state)))
    let newAction = do
            (_, cancelTimer) <- liftIO action
            nextState fopData cancelTimer
    pure (newst, newAction)



suspendAD
    :: (MonadIO m, MonadReader env m, HasGlobalState env)
    => env
    -> IO Bool
    -> FOPData
    -> FOPState
    -> State
    -> STM (FOPState, m ())
suspendAD env cancelTimer fopData fops state = do
    let vcid = fopData ^. fvcid
    (newst, action) <- alertSTM (EVSuspendedAD vcid state)
                                stateInactive
                                env
                                vcid
                                cancelTimer
                                fopData
                                fops
    let newst2    = newst & fopSuspendState .~ state
        newAction = do
            action
            stateInactive fopData cancelTimer
    pure (newst2, newAction)



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
                        seg1 <- takeTMVar (fopData ^. fwaitQueue)
                        case seg ^. encSegFlag of
                            SegmentStandalone -> do
                                putTMVar (fopData ^. fwaitQueue) seg1
                                pure True
                            SegmentFirst -> do
                                putTMVar (fopData ^. fwaitQueue) seg1
                                pure True
                            _ -> loop
                loop

purgeSentQueue :: FOPState -> FOPState
purgeSentQueue fops = fops & fopSentQueue .~ S.empty


alertSTM
    :: (MonadIO m, HasGlobalState env)
    => EventCOP1
    -> (FOPData -> IO Bool -> m ())
    -> env
    -> VCID
    -> IO Bool
    -> FOPData
    -> FOPState
    -> STM (FOPState, m ())
alertSTM event newState env _vcid cancelTimer fopData fops = do
    let newst  = purgeSentQueue fops
        action = do
            liftIO $ do
                void cancelTimer
                raiseEvent env (EVCOP1 event)
            newState fopData cancelTimer

    void $ purgeWaitQueue fopData
    pure (newst, action)



-- | this is the whole state machine of the FOP-1 starting point
fop1Program
    :: (MonadIO m, MonadReader env m, HasGlobalState env) => FOPData -> m ()
fop1Program fopData = do
    stateInactive fopData (pure True)


stateInactive
    :: (MonadIO m, MonadReader env m, HasGlobalState env)
    => FOPData
    -> IO Bool
    -> m ()
stateInactive fopData cancelTimer = do
    inp' <- liftIO $ atomically $ readCOP1Q (fopData ^. fcop1Queue)
    case inp' of
        COP1Dir dir -> case dir of
            InitADWithoutCLCW -> do
                initADWithoutCLCW fopData
                stateActive fopData cancelTimer
            InitADWithCLCW -> do
                cancelTimer' <- initADWithCLCW fopData
                stateInitialisingWithoutBC fopData cancelTimer'
            InitADWithUnlock Unlock -> do
                (newst, cancelTimer') <- initADWithUnlock fopData
                case newst of
                    InitialisingWithBC ->
                        stateInitialisingWithBC fopData cancelTimer'
                    Initial -> stateInactive fopData cancelTimer'
                    _       -> stateInactive fopData cancelTimer'
            InitADWithSetVR setVR -> do
                newst <- initADWithSetVR fopData setVR
                case newst of
                    InitialisingWithBC ->
                        stateInitialisingWithBC fopData cancelTimer
                    Initial -> stateInactive fopData cancelTimer
                    _       -> stateInactive fopData cancelTimer
            TerminateAD -> stateInactive fopData cancelTimer
            ResumeAD    -> do
                (newst, cancelTimer') <- resumeAD fopData
                case newst of
                    Initial -> stateInactive fopData cancelTimer'
                    Active  -> stateActive fopData cancelTimer'
                    RetransmitWithoutWait ->
                        stateRetransmitWithoutWait fopData cancelTimer'
                    RetransmitWithWait ->
                        stateRetransmitWithWait fopData cancelTimer'
                    InitialisingWithoutBC ->
                        stateInitialisingWithoutBC fopData cancelTimer'
                    InitialisingWithBC ->
                        stateInitialisingWithBC fopData cancelTimer'
            SetVS vs -> do
                setMemberAndConfirm
                    fopData
                    (\sta -> sta & fopVS .~ vs & fopNNR .~ vs)
                    EVADConfirmSetVS
                    vs
                stateInactive fopData cancelTimer
            SetFOPSlidingWindowWidth sww -> do
                setMemberAndConfirm fopData
                                    (fopSlidingWinWidth .~ sww)
                                    EVADConfirmSetSlidingWinWidth
                                    sww
                stateInactive fopData cancelTimer
            SetT1Initial t1 -> do
                setMemberAndConfirm fopData
                                    (fopT1Initial .~ t1)
                                    EVADConfirmSetT1Initial
                                    t1
                stateInactive fopData cancelTimer
            SetTransmissionLimit tl -> do
                setMemberAndConfirm fopData
                                    (fopTransmissionLimit .~ tl)
                                    EVADConfirmSetTransmissionLimit
                                    tl
                stateInactive fopData cancelTimer
            SetTimeoutType tt -> do
                setMemberAndConfirm fopData
                                    (fopTimeoutType .~ tt)
                                    EVADConfirmSetTimeoutType
                                    tt
                stateInactive fopData cancelTimer
            _ -> stateInactive fopData cancelTimer
        COP1CLCW _clcw -> stateInactive fopData cancelTimer
        COP1Timeout    -> stateInactive fopData cancelTimer

    pure ()


setMemberAndConfirm
    :: (MonadIO m, MonadReader env m, HasGlobalState env)
    => FOPData
    -> (FOPState -> FOPState)
    -> (VCID -> t -> EventCOP1)
    -> t
    -> m ()
setMemberAndConfirm fopData setOp event val = do
    env <- ask
    join $ withFOPState fopData $ \state -> do
        let newst = setOp state
            action =
                liftIO $ raiseEvent env $ EVCOP1 (event (fopData ^. fvcid) val)
        pure (newst, action)



sendBCFrameSTM :: Config -> FOPData -> FOPState -> TCDirective -> STM FOPState
sendBCFrameSTM cfg fopData fopState directive = do
    let vcid         = fopData ^. fvcid
        scid         = cfgSCID cfg
        encDirective = builderBytes $ directiveBuilder directive
-- create a new BC transfer frame and fill out the values
        frame        = TCTransferFrame { _tcFrameVersion = 0
                                       , _tcFrameFlag    = FrameBC
                                       , _tcFrameSCID    = scid
                                       , _tcFrameVCID    = vcid
                                       , _tcFrameLength  = 0
                                       , _tcFrameSeq     = fopState ^. fopVS
                                       , _tcFrameData    = encDirective
                                       }
        trans = TCFrameTransport frame rqst
        rqst  = TCRequest 0 IF_NCTRS scid vcid (TCDir directive)

-- create a new state which has V(S) incremented
        newst = fopState & fopVS +~ 1 & addToSentQueue trans False

    -- write the frame to the out queue
    writeTBQueue (fopData ^. fout) trans

    pure newst


addToSentQueue :: TCFrameTransport -> Bool -> FOPState -> FOPState
addToSentQueue frame retrans state =
    state
        & over fopSentQueue (S.|> (frame, retrans))
        & if S.null (state ^. fopSentQueue)
              then fopTransmissionCount .~ 1
              else id

-- | send a BC Frame to the out channel (to the interface for encoding and sending).
-- Creates a new BC Frame with the given directive as content, encodes the directive,
-- fills out the frame values, increments the V(S) counter of the COP-1 state machine
-- and sends the frame out to the channel.
_sendBCFrame
    :: (MonadIO m, MonadReader env m, HasGlobalState env)
    => FOPData
    -> TCDirective
    -> m ()
_sendBCFrame fopData directive = do
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
        let frame = TCTransferFrame { _tcFrameVersion = 0
                                    , _tcFrameFlag    = FrameBC
                                    , _tcFrameSCID    = scid
                                    , _tcFrameVCID    = vcid
                                    , _tcFrameLength  = 0
                                    , _tcFrameSeq     = st ^. fopVS
                                    , _tcFrameData    = encDirective
                                    }
            trans = TCFrameTransport frame rqst
            rqst  = TCRequest 0 IF_NCTRS scid vcid (TCDir directive)

-- create a new state which has V(S) incremented
            newst = st & fopVS +~ 1

        -- write the new state
        writeTVar fopState newst
        -- write the frame to the out queue
        writeTBQueue (fopData ^. fout) trans



stateActive
    :: (MonadIO m, MonadReader env m, HasGlobalState env)
    => FOPData
    -> IO Bool
    -> m ()
stateActive fopData cancelTimer = do
    pure ()


-- | S2
stateRetransmitWithoutWait
    :: (MonadIO m, MonadReader env m, HasGlobalState env)
    => FOPData
    -> IO Bool
    -> m ()
stateRetransmitWithoutWait fopData cancelTimer = do
    pure ()


-- | S3
stateRetransmitWithWait
    :: (MonadIO m, MonadReader env m, HasGlobalState env)
    => FOPData
    -> IO Bool
    -> m ()
stateRetransmitWithWait fopData cancelTimer = do
    pure ()

type Signature env m
    = --(MonadIO m, MonadReader env m, HasGlobalState env) =>
      env -> VCID -> IO Bool -> FOPData -> FOPState -> STM (FOPState, m ())

-- | This structure correspondes to the transitions
-- defined in the FOP-1 state transition table in the
-- PSS Standard (ESA PSS-04-107 Issue 2)
data StateActions env m = StateActions {
    e1 :: Signature env m
    , e2 :: Signature env m
    , e3 :: Signature env m
    , e4 :: Signature env m
    , e5 :: Signature env m
    , e6 :: Signature env m
    , e7 :: Signature env m
    , e8 :: Signature env m
    , e9 :: Signature env m
    , e10 :: Signature env m
    , e11 :: Signature env m
    , e12 :: Signature env m
    , e13 :: Signature env m
    , e14 :: Signature env m
    , e15 :: Text -> Signature env m -- error message from CLCW extraction
    , e16 :: Word8 -> Word8 -> State -> Signature env m
    , e17 :: Word8 -> Word8 -> State -> Signature env m
    , e18 :: Signature env m
    , e19 :: Signature env m
    , e20 :: Signature env m
    , e21 :: Signature env m
    , e22 :: Signature env m
    , e23 :: Signature env m
    , e24 :: Signature env m
    , e25 :: Signature env m
    , e26 :: Signature env m
    , e27 :: Word8 -> Signature env m
    , e28 :: Word8 -> Signature env m
    , e29 :: Signature env m
    , e30 :: Signature env m
    , e31 :: Signature env m
    , e32 :: Signature env m
    , e33 :: Signature env m
    , e34 :: Signature env m
    }


doNothing
    :: --(MonadIO m, MonadReader env m, HasGlobalState env) =>
       (FOPData -> IO Bool -> m ()) -> Signature env m
doNothing newState _env _vcid cancelTimer fopData fops =
    pure (fops, newState fopData cancelTimer)

reject ::(MonadIO m, MonadReader env m, HasGlobalState env) =>
    Text
    -> (FOPData -> IO Bool -> m ())
    -> env
    -> VCID
    -> IO Bool
    -> FOPData
    -> FOPState
    -> STM (FOPState, m())
reject msg newState env vcid cancelTimer fopData fops =
    alertSTM (EVReject vcid msg) newState env vcid cancelTimer fopData fops


-- | S4
stateInitialisingWithoutBC
    :: (MonadIO m, MonadReader env m, HasGlobalState env)
    => FOPData
    -> IO Bool
    -> m ()
stateInitialisingWithoutBC fopData cancelTimer = do
    env <- ask
    let !vcid        = fopData ^. fvcid

        stateActions = StateActions
            { e1  = \_ _ cancelTimer' fopData' fops -> do
                        let action = do
                                void $ liftIO cancelTimer'
                                stateActive fopData' cancelTimer'
                        pure (fops, action)
            , e2  = \_ _ cancelTimer' fopData' fops ->
                        pure
                            ( fops
                            , stateInitialisingWithoutBC fopData' cancelTimer'
                            )
            , e3  = alertWaitAction
            , e4  =
                alertSTM
                    (EVADAlert
                        "Received RETRANSMIT while in state Initialising Without BC"
                    )
                    stateInactive
            , e5  = doNothing stateInitialisingWithoutBC
            , e6  = alertNrNnrNotEqual
            , e7  = alertWaitAction
            , e8  = alertNrNnrNotEqual
            , e9  = alertNrNnrNotEqual
            , e10 = doNothing stateInitialisingWithoutBC
            , e11 = doNothing stateInitialisingWithoutBC
            , e12 = doNothing stateInitialisingWithoutBC
            , e13 = alertSTM (EVCLCWIllegalNR vcid InitialisingWithoutBC)
                             stateInactive
            , e14 = alertSTM (EVLockout vcid InitialisingWithoutBC)
                             stateInactive
            , e15 = \err env' vcid' cancelTimer' fopData' fops -> alertSTM
                        (EVADAlert err)
                        stateInactive
                        env'
                        vcid'
                        cancelTimer'
                        fopData'
                        fops
            , e16 = \transC transL st env' vcid' cancelTimer' fopData' fops ->
                        alertSTM (EVADTransLimit vcid transC transL st)
                                 stateInactive
                                 env'
                                 vcid'
                                 cancelTimer'
                                 fopData'
                                 fops
            , e17 = \transC transL st env' vcid' cancelTimer' fopData' fops ->
                        alertSTM (EVADTransLimit vcid transC transL st)
                                 stateInactive
                                 env'
                                 vcid'
                                 cancelTimer'
                                 fopData'
                                 fops
            , e18 = \env' vcid' cancelTimer' fopData' fops -> suspendAD
                        env'
                        cancelTimer'
                        fopData'
                        fops
                        InitialisingWithoutBC
            , e19 = doNothing stateInitialisingWithoutBC
            , e20 = doNothing stateInitialisingWithoutBC
            , e21 = doNothing stateInitialisingWithoutBC
            , e22 = doNothing stateInitialisingWithoutBC
            , e23 = doNothing stateInitialisingWithoutBC
            , e24 = doNothing stateInitialisingWithoutBC
            , e25 = doNothing stateInitialisingWithoutBC
            , e26 = doNothing stateInitialisingWithoutBC
            , e27 = \_ -> doNothing stateInitialisingWithoutBC
            , e28 = \_ -> doNothing stateInitialisingWithoutBC
            , e29 = terminateAD InitialisingWithoutBC
            , e30 = doNothing stateInitialisingWithoutBC
            , e31 = doNothing stateInitialisingWithoutBC
            , e32 = doNothing stateInitialisingWithoutBC
            , e33 = doNothing stateInitialisingWithoutBC
            , e34 = doNothing stateInitialisingWithoutBC
            }

    inp <- atomically $ readCOP1Q (fopData ^. fcop1Queue)
    case inp of
        COP1CLCW clcw -> do
            join $ withFOPState fopData $ \fops -> do
                case checkCLCW clcw of
                    Left err ->
                        e15 stateActions err env vcid cancelTimer fopData fops
                    Right _ -> processCLCW env
                                           vcid
                                           cancelTimer
                                           fopData
                                           fops
                                           stateActions
                                           clcw
        COP1Timeout -> do
            join $ withFOPState fopData $ \fops -> do
                let transCount = fops ^. fopTransmissionCount
                    transLimit = fops ^. fopTransmissionLimit
                if transCount < transLimit
                    then e16 stateActions
                             transCount
                             transLimit
                             InitialisingWithoutBC
                             env
                             vcid
                             cancelTimer
                             fopData
                             fops
                    else case fops ^. fopTimeoutType of
                        TTAlert -> e17 stateActions
                                       transCount
                                       transLimit
                                       InitialisingWithoutBC
                                       env
                                       vcid
                                       cancelTimer
                                       fopData
                                       fops
                        TTSuspend -> e18 stateActions
                                         env
                                         vcid
                                         cancelTimer
                                         fopData
                                         fops
        COP1Dir dir -> do
            join $ withFOPState fopData $ \fops -> do
                processDirective dir
                                 env
                                 vcid
                                 cancelTimer
                                 fopData
                                 fops
                                 stateActions
    pure ()

processCLCW
    :: (MonadIO m, MonadReader env m, HasGlobalState env)
    => env
    -> VCID
    -> IO Bool
    -> FOPData
    -> FOPState
    -> StateActions env m
    -> CLCW
    -> STM (FOPState, m ())
processCLCW env vcid cancelTimer fopData fops stateActions clcw =
    if clcw ^. clcwLockout
        then e14 stateActions env vcid cancelTimer fopData fops
        else checkReportVal env vcid cancelTimer fopData fops stateActions clcw

checkReportVal
    :: (MonadIO m, MonadReader env m, HasGlobalState env)
    => env
    -> VCID
    -> IO Bool
    -> FOPData
    -> FOPState
    -> StateActions env m
    -> CLCW
    -> STM (FOPState, m ())
checkReportVal env vcid cancelTimer fopData fops stateActions clcw = do
    -- Lockout == False, check N(R) and V(S)
    let nR  = clcw ^. clcwReportVal
        vS  = fops ^. fopVS
        nNR = fops ^. fopNNR
    if
        | nR == vS -> checkRetransmit env
                                      vcid
                                      cancelTimer
                                      fopData
                                      fops
                                      stateActions
                                      clcw
        | nR < vS && nR >= nNR -> checkRetransmitNRVS env
                                                      vcid
                                                      cancelTimer
                                                      fopData
                                                      fops
                                                      stateActions
                                                      clcw
        | nR < nNR || nR > vS -> e13 stateActions
                                     env
                                     vcid
                                     cancelTimer
                                     fopData
                                     fops
        | otherwise -> pure
            (fops, stateInitialisingWithoutBC fopData cancelTimer) -- not applicable in this state

checkRetransmit
    ::
    -- :: (MonadIO m, MonadReader env m, HasGlobalState env)
       env
    -> VCID
    -> IO Bool
    -> FOPData
    -> FOPState
    -> StateActions env m
    -> CLCW
    -> STM (FOPState, m ())
checkRetransmit env vcid cancelTimer fopData fops stateActions clcw =
    -- check the retransmit flag
    if clcw ^. clcwRetrans
        then e4 stateActions env vcid cancelTimer fopData fops
        else checkWait env vcid cancelTimer fopData fops stateActions clcw

checkWait
    :: -- (MonadIO m, MonadReader env m, HasGlobalState env) =>
       env
    -> VCID
    -> IO Bool
    -> FOPData
    -> FOPState
    -> StateActions env m
    -> CLCW
    -> STM (FOPState, m ())
checkWait env vcid cancelTimer fopData fops stateActions clcw
    | clcw ^. clcwWait = e3 stateActions env vcid cancelTimer fopData fops
    | clcw ^. clcwReportVal == fops ^. fopNNR = e1 stateActions
                                                   env
                                                   vcid
                                                   cancelTimer
                                                   fopData
                                                   fops
    | otherwise = e2 stateActions env vcid cancelTimer fopData fops

alertWaitAction
    :: (MonadIO m, MonadReader env m, HasGlobalState env)
    => env
    -> VCID
    -> IO Bool
    -> FOPData
    -> FOPState
    -> STM (FOPState, m ())
alertWaitAction env vcid cancelTimer fopData fops = alertSTM
    (EVADCLCWWait vcid True InitialisingWithoutBC)
    stateInactive
    env
    vcid
    cancelTimer
    fopData
    fops


checkRetransmitNRVS
    ::  -- (MonadIO m, MonadReader env m, HasGlobalState env) =>
       env
    -> VCID
    -> IO Bool
    -> FOPData
    -> FOPState
    -> StateActions env m
    -> CLCW
    -> STM (FOPState, m ())
checkRetransmitNRVS env vcid cancelTimer fopData fops stateActions clcw = do
    if clcw ^. clcwRetrans
        then if clcw ^. clcwReportVal /= fops ^. fopNNR
            then chkWait
            else chkTrans
        else checkWaitNRVS env vcid cancelTimer fopData fops stateActions clcw
  where
    chkWait = if clcw ^. clcwWait
        then e9 stateActions env vcid cancelTimer fopData fops
        else e8 stateActions env vcid cancelTimer fopData fops
    chkTrans = if fops ^. fopTransmissionCount < fops ^. fopTransmissionLimit
        then if clcw ^. clcwWait
            then e11 stateActions env vcid cancelTimer fopData fops
            else e10 stateActions env vcid cancelTimer fopData fops
        else e12 stateActions env vcid cancelTimer fopData fops


checkWaitNRVS
    ::  --(MonadIO m, MonadReader env m, HasGlobalState env)
       env
    -> VCID
    -> IO Bool
    -> FOPData
    -> FOPState
    -> StateActions env m
    -> CLCW
    -> STM (FOPState, m ())
checkWaitNRVS env vcid cancelTimer fopData fops stateActions clcw
    | clcw ^. clcwWait = e7 stateActions env vcid cancelTimer fopData fops
    | clcw ^. clcwReportVal == fops ^. fopNNR = e5 stateActions
                                                   env
                                                   vcid
                                                   cancelTimer
                                                   fopData
                                                   fops
    | otherwise = e6 stateActions env vcid cancelTimer fopData fops

alertNrNnrNotEqual
    :: (MonadIO m, MonadReader env m, HasGlobalState env)
    => env
    -> VCID
    -> IO Bool
    -> FOPData
    -> FOPState
    -> STM (FOPState, m ())
alertNrNnrNotEqual env vcid cancelTimer fopData fops = do
    alertSTM (EVNrNnrNotEqual vcid InitialisingWithoutBC)
             stateInactive
             env
             vcid
             cancelTimer
             fopData
             fops

processDirective
    :: (MonadIO m, MonadReader env m, HasGlobalState env) =>
       COP1Directive
    -> env
    -> VCID
    -> IO Bool
    -> FOPData
    -> FOPState
    -> StateActions env m
    -> STM (FOPState, m ())
processDirective dir env vcid cancelTimer fopData fops stateActions = do
    case dir of
        InitADWithoutCLCW -> e23 stateActions env vcid cancelTimer fopData fops
        InitADWithCLCW    -> e24 stateActions env vcid cancelTimer fopData fops
        InitADWithUnlock _ ->
            e25 stateActions env vcid cancelTimer fopData fops
        InitADWithSetVR (SetVR vr) ->
            e27 stateActions vr env vcid cancelTimer fopData fops
        TerminateAD -> e29 stateActions env vcid cancelTimer fopData fops
        ResumeAD    -> processResume env vcid cancelTimer fopData fops stateActions
        -- SetVS !Word8 ->
        -- SetFOPSlidingWindowWidth !Word8 ->
        -- SetT1Initial (Fixed E6) ->
        -- SetTransmissionLimit !Word8 ->
        -- SetTimeoutType TTType ->

terminateAD
    :: (MonadIO m, MonadReader env m, HasGlobalState env)
    => State
    -> env
    -> VCID
    -> IO Bool
    -> FOPData
    -> FOPState
    -> STM (FOPState, m ())
terminateAD state env vcid =
    alertSTM (EVTerminatedAD vcid state) stateInactive env vcid


-- | S5
stateInitialisingWithBC fopData cancelTimer = do
    pure ()


readSegment :: (MonadIO m) => TMVar EncodedSegment -> m EncodedSegment
readSegment var = atomically $ takeTMVar var


readInput :: (MonadIO m) => COP1Queue -> m COP1Input
readInput chan = atomically $ readTBQueue chan





notifyTimeout :: FOPData -> IO ()
notifyTimeout fopData =
    atomically $ sendCOP1Q (fopData ^. fcop1Queue) COP1Timeout



-- | This is a conduit for the input. @chan@ is a 'TBQueue' which is wrapped in
-- a input conduit (STM Conduit). @segBuffer@ is a 'TMVar' used as the /waitQueue/
-- for the COP-1 protocol. @outQueue@ is the direct output queue of the output interface,
-- in most cases this will be NCTRS or SLE. The @outQueue@ is only needed in BD mode.
-- If a encoded BD segment is received, it is directly forwarded to the interface via
-- the TBQueue without going through the COP-1 machine.
--
-- __Note:__ this is a bit against the standard, which says that if a BD frame is
--           transmitted in between AD frames, the AD mode should be terminated.
--           But, actually SCOS-2000 behaves this way (it doesn't terminate the AD
--           mode in this case), so we keep it like this. This can be changed should
--           the situation require it
--
-- So basically, this conduit gets encoded segments wrapped in ProtocolPackets as
-- input and places them in the TMVar. This may block if the COP-1 machine is busy
-- or in a state where it doesn't accept commands, which is the expected wait queue
-- behaviour. The COP-1 state machine will read the segment from the TMVar when
-- it is ready and forward it to another channel for TC Transfer Frame, CLTU and
-- finally NCTRS or SLE encoding.
cop1Conduit
    :: (MonadIO m, MonadReader env m, HasConfig env)
    => NCTRSChan EncodedSegment -- ^ Input Conduit for the encoded segments
    -> TMVar EncodedSegment     -- ^ Wait Queue of the COP-1 state machine
    -> TBQueue TCFrameTransport  -- ^ Direct output for BD frames
    -> ConduitT () Void m ()
cop1Conduit chan segBuffer outQueue = do
    cfg <- view getConfig
    sourceTBQueue chan .| proc cfg
  where
    proc :: (MonadIO m) => Config -> ConduitT EncodedSegment Void m ()
    proc cfg = do
        x <- await
        case x of
            Nothing  -> pure ()
            Just seg -> do
                -- check the actual transmission mode.
                case seg ^. encSegRequest . to tcReqTransmissionMode of
                    -- AD mode. Put the segment into the wait queue for the COP-1 protocol machine
                    AD -> do
                        atomically $ putTMVar segBuffer seg
                        proc cfg
                    -- BD mode. Directly create a BD transfer frame and send it to the out queue
                    BD -> do
                        let vcid  = seg ^. encSegRequest . tcReqVCID
                            scid  = cfgSCID cfg
-- create a new BD mode transfer frame and fill out the values
                            frame = TCTransferFrame
                                { _tcFrameVersion = 0
                                , _tcFrameFlag    = FrameBD
                                , _tcFrameSCID    = scid
                                , _tcFrameVCID    = vcid
                                , _tcFrameLength  = 0
                                , _tcFrameSeq     = 0
                                , _tcFrameData    = seg ^. encSegSegment
                                }
                            trans =
                                TCFrameTransport frame (seg ^. encSegRequest)
                        atomically $ writeTBQueue outQueue trans
                        proc cfg
