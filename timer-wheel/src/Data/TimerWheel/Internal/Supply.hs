module Data.TimerWheel.Internal.Supply
  ( Supply,
    new,
    next,
  )
where

import Data.Atomics.Counter (AtomicCounter, incrCounter, newCounter)
import Data.Coerce (coerce)

newtype Supply
  = Supply AtomicCounter

new :: IO Supply
new =
  coerce (newCounter 0)

next :: Supply -> IO Int
next =
  coerce (incrCounter 1)
{-# INLINE next #-}
