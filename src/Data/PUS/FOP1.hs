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

import           Control.Lens                   ( (.~) )

--import           UnliftIO.STM

import           Control.PUS.Classes

import           Data.PUS.COP1Types
--import           Data.PUS.TCTransferFrame
--import           Data.PUS.CLCW
import           Data.PUS.Types
--import           Data.PUS.Time
--import           Data.PUS.TCDirective
import           Data.PUS.Segment
--import           Data.PUS.GlobalState
import           Data.PUS.Events

import           Protocol.Switcher
import           Protocol.ProtocolInterfaces




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

  initADWithoutCLCW :: (MonadIO m, MonadReader env m, HasFOPState env) => VCID -> State m Initial -> m (State m Active)
  initADWithCLCW :: (MonadIO m, MonadReader env m, HasFOPState env) => VCID -> State m Initial -> m (State m InitialisingWithoutBC)
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

    initADWithoutCLCW vcid _ = do
        st <- fopStateG vcid <$> ask
        atomically $
            modifyTVar' st (\state -> state & fopWaitFlag .~ False & fopLockoutFlag .~ False & fopRetransmitFlag .~ False
                & fopSentQueue .~ S.empty & fopToBeRetransmitted .~ False
                & fopADout .~ toFlag Ready True
                & fopBDout .~ toFlag Ready True
                & fopBCout .~ toFlag Ready True
                & fopTransmissionCount .~ 0)
        pure Active

    initADWithCLCW vcid _ = do
        st <- fopStateG vcid <$> ask
        atomically $
          modifyTVar' st (\state -> state & fopWaitFlag .~ False & fopLockoutFlag .~ False & fopRetransmitFlag .~ False
            & fopSentQueue .~ S.empty & fopToBeRetransmitted .~ False
            & fopADout .~ toFlag Ready True
            & fopBDout .~ toFlag Ready True
            & fopBCout .~ toFlag Ready True
            & fopTransmissionCount .~ 0)
        pure InitialisingWithoutBC


fop1Program
    :: (MonadIO m, MonadReader env m, HasGlobalState env, FOPMachine m)
    => VCID
    -> MVar EncodedSegment
    -> COP1Queue
    -> m ()
fop1Program vcid segBuffer cop1Queue = do
    st <- initial
    stateInactive vcid st cop1Queue


stateInactive
    :: (MonadIO m, MonadReader env m, HasGlobalState env, FOPMachine m)
    => VCID
    -> State m Initial
    -> COP1Queue
    -> m ()
stateInactive vcid st cop1Queue = do
    inp <- liftIO $ readInput cop1Queue
    case inp of
        COP1Dir dir -> case dir of
            InitADWithoutCLCW -> do
                newst <- initADWithoutCLCW vcid st
                env   <- ask
                liftIO $ raiseEvent env $ EVCOP1
                    (EV_ADInitializedWithoutCLCW vcid)
                -- go to next state
                stateActive vcid newst
            InitADWithCLCW -> do
                newst <- initADWithCLCW vcid st
                env   <- ask
                liftIO $ raiseEvent env $ EVCOP1 (EV_ADInitWaitingCLCW vcid)
                stateInitialisingWithoutBC vcid newst
                pure ()
        COP1CLCW clcw -> pure ()
    pure ()


stateActive vcid st = do
    pure ()


-- | S2
stateRetransmitWithoutWait vcid st = do
    pure ()


    -- | S3
stateRetransmitWithWait vcid st = do
    pure ()


    -- | S4
stateInitialisingWithoutBC vcid st = do
    pure ()


    -- | S5
stateInitialisingWithBC vcid st = do
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
