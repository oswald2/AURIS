module Protocol.Internal.SLEDummy
    ( startSLE
    ) where

import           RIO

import           Data.PUS.Config

startSLE :: (Monad m) => SLEConfig -> m ()
startSLE _sleCfg = pure ()
