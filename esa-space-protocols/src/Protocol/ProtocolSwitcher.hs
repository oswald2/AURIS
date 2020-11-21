module Protocol.ProtocolSwitcher
  ( InterfaceSwitcherMap
  , QueueMsg(..)
  , ProtocolQueue
  , switchProtocolPktC
  , switchProtocolFrameC
  , switchProtocolCltuC
  , receivePktChannelC
  , receiveFrameChannelC
  , receiveCltuChannelC
  , receiveQueueMsg
  , createInterfaceChannel
  )
where


import           RIO
import qualified RIO.HashMap                   as HM

import           Conduit                       as C
import Data.Conduit.TQueue 

import           Data.PUS.PUSPacketEncoder

import Data.PUS.CLTU ( EncodedCLTU(cltuRequest) ) 
import Data.PUS.TCFrameTypes ( encTcFrameRequest, EncodedTCFrame )

import Protocol.ProtocolInterfaces
    ( ProtocolDestination(destination), ProtocolInterface )   



-- | The EDEN queue for the given interfaces can encode different protocol levels, 
-- therefore this sum type allows to specify the final content of the EDEN message to be
-- sent.
data QueueMsg =
  EQPacket EncodedPUSPacket
  | EQFrame EncodedTCFrame
  | EQCLTU EncodedCLTU

-- | The type of the queue used to forward TCs to the correct interfaces with the 
-- intended interface.
type ProtocolQueue = TBQueue QueueMsg

-- | A HashMap containing the channels where the messages should be sent for EDEN. 
type InterfaceSwitcherMap = HashMap ProtocolInterface ProtocolQueue


-- | The queueSize for the used 'TBQueue'. Defaults to 500
queueSize :: Natural
queueSize = 500


-- | Creates a new 'ProtocolQueue', inserts it into a given 'InterfaceSwitcherMap' and 
-- returns both. Needed for the thread creation of the interfaces. 
createInterfaceChannel :: (MonadIO m) => InterfaceSwitcherMap -> ProtocolInterface -> m (ProtocolQueue, InterfaceSwitcherMap)
createInterfaceChannel sm interf = do 
  q <- newTBQueueIO queueSize 
  return (q, HM.insert interf q sm)


-- | This is an endpoint conduit which forwards an encoded packet ('EncodedPUSPacket')
-- to the right channel for further processing
switchProtocolPktC
  :: (MonadIO m, MonadReader env m, HasLogFunc env)
  => InterfaceSwitcherMap
  -> ConduitT EncodedPUSPacket Void m ()
switchProtocolPktC sm = awaitForever $ \pkt -> do
  let dest = destination (pkt ^. encPktRequest)
  case HM.lookup dest sm of
    Just entry -> atomically $ writeTBQueue entry (EQPacket pkt)
    Nothing ->
      logError
        $  "ProtocolSwitcher: could not find interface specified in command: "
        <> displayShow dest

-- | This is an endpoint conduit which forwards an encoded frame ('EncodedTCFrame')
-- to the right channel for further processing
switchProtocolFrameC
  :: (MonadIO m, MonadReader env m, HasLogFunc env)
  => InterfaceSwitcherMap
  -> ConduitT EncodedTCFrame Void m ()
switchProtocolFrameC sm = awaitForever $ \frame -> do
  let dest = destination (frame ^. encTcFrameRequest)
  case HM.lookup dest sm of
    Just entry -> atomically $ writeTBQueue entry (EQFrame frame)
    Nothing ->
      logError
        $  "ProtocolSwitcher: could not find interface specified in command: "
        <> displayShow dest


-- | This is an endpoint conduit which forwards an encoded CLTU ('EncodedCLTU')
-- to the right channel for further processing
switchProtocolCltuC
  :: (MonadIO m, MonadReader env m, HasLogFunc env)
  => InterfaceSwitcherMap
  -> ConduitT EncodedCLTU Void m ()
switchProtocolCltuC sm = awaitForever $ \cltu -> do
  let dest = destination (cltuRequest cltu) 
  case HM.lookup dest sm of
    Just entry -> atomically $ writeTBQueue entry (EQCLTU cltu)
    Nothing ->
      logError
        $  "ProtocolSwitcher: could not find interface specified in command: "
        <> displayShow dest


-- | Conduit which receives a 'QueueMsg' via a STM 'TBQueue' and forwards 'EncodePUSPacket's.
-- Other types are discarded. 
receivePktChannelC :: (MonadIO m) => ProtocolQueue -> ConduitT () EncodedPUSPacket m ()
receivePktChannelC queue = sourceTBQueue queue .| unwrap
  where 
    unwrap = awaitForever $ \case 
      EQPacket pkt -> yield pkt 
      EQFrame _ -> return () 
      EQCLTU _ -> return () 

-- | Conduit which receives a 'QueueMsg' via a STM 'TBQueue' and forwards 'EncodedTCFrame's.
-- Other types are discarded. 
receiveFrameChannelC :: (MonadIO m) => ProtocolQueue -> ConduitT () EncodedTCFrame m ()
receiveFrameChannelC queue = sourceTBQueue queue .| unwrap
  where 
    unwrap = awaitForever $ \case 
      EQPacket _ -> return ()
      EQFrame frame -> yield frame 
      EQCLTU _ -> return () 

-- | Conduit which receives a 'QueueMsg' via a STM 'TBQueue' and forwards 'EncodedCLTU's.
-- Other types are discarded. 
receiveCltuChannelC :: (MonadIO m) => ProtocolQueue -> ConduitT () EncodedCLTU m ()
receiveCltuChannelC queue = sourceTBQueue queue .| unwrap
  where 
    unwrap = awaitForever $ \case 
      EQPacket _ -> return ()
      EQFrame _ -> return () 
      EQCLTU cltu -> yield cltu 

-- | Conduit which receives a 'QueueMsg' via a STM 'TBQueue' and simply yields it.
receiveQueueMsg :: (MonadIO m) => ProtocolQueue -> ConduitT () QueueMsg m () 
receiveQueueMsg = sourceTBQueue
