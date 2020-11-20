module Data.PUS.ProtocolSwitcher
  ( InterfaceSwitcherMap
  , switchProtocolC
  )
where


import           RIO
import qualified RIO.HashMap                   as HM

import           Conduit                       as C

import           Data.PUS.PUSPacketEncoder
import           Data.PUS.TCRequest

import           Protocol.ProtocolInterfaces



type InterfaceSwitcherMap = HashMap ProtocolInterface (TBQueue EncodedPUSPacket)



switchProtocolC
  :: (MonadIO m, MonadReader env m, HasLogFunc env) => InterfaceSwitcherMap -> ConduitT EncodedPUSPacket Void m ()
switchProtocolC sm = awaitForever $ \pkt -> do
  let dest = pkt ^. encPktRequest . tcReqDestination
  case HM.lookup dest sm of
    Just entry -> atomically $ writeTBQueue entry pkt
    Nothing ->
      logError $
          "ProtocolSwitcher: could not find interface specified in command: "
        <> displayShow dest
