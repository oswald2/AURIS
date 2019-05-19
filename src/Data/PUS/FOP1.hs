{-# LANGUAGE GeneralizedNewtypeDeriving
    , BangPatterns
    , NoImplicitPrelude
    , TemplateHaskell
    , OverloadedStrings
    , GADTs
    , TypeFamilies
#-}
module Data.PUS.FOP1
    ()
where

import           RIO

import           Conduit
import           Data.Conduit.TQueue

import           Control.Lens                   ( makeLenses )
--import           Control.Lens.Setter

import           UnliftIO.STM

--import qualified Data.ByteString.Lazy          as B

import           Control.PUS.Classes

import           Data.PUS.COP1Types
import           Data.PUS.TCTransferFrame
import           Data.PUS.CLCW
import           Data.PUS.Types
import           Data.PUS.Time
import           Data.PUS.TCDirective
import           Data.PUS.Segment

import           Protocol.Switcher
import           Protocol.ProtocolInterfaces




_checkSlidingWinWidth :: Word8 -> Bool
_checkSlidingWinWidth w = (2 < w) && (w < 254) && even w


data COP1Directive =
  InitADWithoutCLCW
  | InitADWithCLCW
  | InitADWithUnlock TCDirective
  | InitADWithSetVR  TCDirective


data COP1Input =
  COP1Segment EncodedSegment
  | COP1Directive TCDirective
  | COP1CLCW CLCW



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


newtype FOPMachineT m a = FOPTMachineT { runFOPMachineT :: m a }
  deriving (Functor, Monad, Applicative, MonadIO)


data FOPMachineState s where
  Active :: FOPMachineState Active
  RetransmitWithoutWait :: FOPMachineState RetransmitWithoutWait
  RetransmitWithWait :: FOPMachineState RetransmitWithWait
  InitialisingWithoutBC :: FOPMachineState InitialisingWithoutBC
  InitialisingWithBC :: FOPMachineState InitialisingWithBC
  Initial :: FOPMachineState Initial


instance (MonadIO m) => FOPMachine (FOPMachineT m) where
  type State (FOPMachineT m) = FOPMachineState

  initial = return Initial


fop1Program :: (Monad m, FOPMachine m) => m ()
fop1Program = do
    initial
    pure ()




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
