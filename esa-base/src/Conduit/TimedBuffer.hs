module Conduit.TimedBuffer
    ( Timeout(..)
    , TimedBuffer
    , newTimedBuffer
    , newTimedBufferIO
    , writeTimedBuffer
    , readTimedBuffer
    ) where

import           RIO

import           Control.Concurrent.STM.TArray

import           Data.Array.MArray


newtype Timeout = Timeout Int 
  deriving newtype (Eq, Ord, Num)


data TimedBuffer a = TimedBuffer
    { array :: !(TArray Word32 a)
    , idx   :: !(TVar Word32)
    , maxN  :: !Word32
    }


newTimedBuffer :: Word32 -> STM (TimedBuffer a)
newTimedBuffer n = do
    arr <- newArray_ (0, n - 1)
    var <- newTVar 0
    pure $! TimedBuffer { array = arr, idx = var, maxN = n }


newTimedBufferIO :: Word32 -> IO (TimedBuffer a)
newTimedBufferIO n = do
    arr <- atomically $ newArray_ (0, n - 1)
    var <- newTVarIO 0
    pure $! TimedBuffer { array = arr, idx = var, maxN = n }


writeTimedBuffer :: (MonadIO m) => TimedBuffer a -> a -> m ()
writeTimedBuffer buf value = atomically $ do
    i <- readTVar (idx buf)
    if i >= (maxN buf)
        then retrySTM
        else do
            writeArray (array buf) i value
            let !nextI = i + 1
            writeTVar (idx buf) nextI



readTimedBuffer :: (MonadIO m) => Timeout -> TimedBuffer a -> m [a]
readTimedBuffer (Timeout timeOut) buf = do 
  delay <- liftIO $ registerDelay timeOut 
  atomically $ do 
    readTimedBufferSTM buf 
    <|> flushTimed delay
  where 
      delayCheck = checkSTM <=< readTVar 

      flushTimed delay = do 
        delayCheck delay 
        flushTimedBufferSTM buf


readTimedBufferSTM :: TimedBuffer a -> STM [a]
readTimedBufferSTM buf = do 
    i <- readTVar (idx buf)
    if i >= (maxN buf)
      then do 
        es <- getElems (array buf)
        writeTVar (idx buf) 0 
        pure es 
      else retrySTM

flushTimedBufferSTM :: TimedBuffer a -> STM [a]
flushTimedBufferSTM buf = do 
    i <- readTVar (idx buf)
    es <- getElems (array buf)
    writeTVar (idx buf) 0
    pure $ take (fromIntegral i) es 


