module Conduit.TimedConduit
    ( timedWriterC
    , timedReaderC
    ) where

import           RIO

import           Conduit

import           Conduit.TimedBuffer





timedWriterC :: (MonadUnliftIO m) => TimedBuffer a -> ConduitT a Void m ()
timedWriterC buf = awaitForever $ \x -> writeTimedBuffer buf x


timedReaderC :: (MonadUnliftIO m) => TimedBuffer a -> Timeout -> ConduitT () [a] m () 
timedReaderC buf timeOut = readTimedBuffer timeOut buf >>= yield 
