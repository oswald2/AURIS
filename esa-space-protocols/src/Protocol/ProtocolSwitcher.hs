module Protocol.ProtocolSwitcher
  ( InterfaceSwitcherMap
  , switchProtocolC
  , createInterfaceSwitcherMap
  )
where


import           RIO
import qualified RIO.HashMap                   as HM

import           Conduit                       as C

import           Data.PUS.PUSPacketEncoder
import           Data.PUS.TCRequest
import           Data.PUS.Config

import           Control.PUS.Classes

import           Protocol.ProtocolInterfaces    ( ProtocolInterface )



type InterfaceSwitcherMap = HashMap ProtocolInterface (TBQueue EncodedPUSPacket)

queueSize :: Natural
queueSize = 500


createInterfaceSwitcherMap
  :: (MonadIO m, MonadReader env m, HasConfig env) => m InterfaceSwitcherMap
createInterfaceSwitcherMap = do
  conf <- view getConfig
  let ifs = getInterfaces conf
      entry i = do
        q <- newTBQueueIO queueSize
        return (i, q)
  HM.fromList <$> mapM entry ifs


switchProtocolC
  :: (MonadIO m, MonadReader env m, HasLogFunc env)
  => InterfaceSwitcherMap
  -> ConduitT EncodedPUSPacket Void m ()
switchProtocolC sm = awaitForever $ \pkt -> do
  let dest = pkt ^. encPktRequest . tcReqDestination
  case HM.lookup dest sm of
    Just entry -> atomically $ writeTBQueue entry pkt
    Nothing ->
      logError
        $  "ProtocolSwitcher: could not find interface specified in command: "
        <> displayShow dest
