module Protocol.Internal.SLEDummy
    ( startSLE
    ) where

import           RIO

import           Data.PUS.Config
import           Data.PUS.TMStoreFrame          ( SwitcherMap )

import           Protocol.ProtocolSLE           ( SLECommand )

startSLE :: (Monad m) => SLEConfig -> SwitcherMap -> TBQueue SLECommand -> m ()
startSLE _sleCfg _switcherMap _queue = pure ()
