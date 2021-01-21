{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

-- | A simple, hashed timer wheel.
module Data.TimerWheel
  ( -- * Timer wheel
    TimerWheel,
    with,
    Config (..),
    register,
    register_,
    recurring,
    recurring_,
  )
where

import Control.Concurrent
import Control.Exception
import Control.Monad (join, void)
import Data.Bool (bool)
import Data.Fixed (E6, Fixed)
import Data.Function (fix)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.TimerWheel.Internal.Config (Config)
import qualified Data.TimerWheel.Internal.Config as Config
import Data.TimerWheel.Internal.Micros (Micros (Micros))
import qualified Data.TimerWheel.Internal.Micros as Micros
import Data.TimerWheel.Internal.Supply (Supply)
import qualified Data.TimerWheel.Internal.Supply as Supply
import Data.TimerWheel.Internal.Wheel (Wheel)
import qualified Data.TimerWheel.Internal.Wheel as Wheel

-- | A timer wheel is a vector-of-collections-of timers to fire. It is configured with a /spoke count/ and /resolution/.
-- Timers may be scheduled arbitrarily far in the future. A timeout thread is spawned to step through the timer wheel
-- and fire expired timers at regular intervals.
--
-- * The /spoke count/ determines the size of the timer vector.
--
--     * A __larger spoke count__ will result in __less insert contention__ at each spoke and will require
--       __more memory__ to store the timer wheel.
--
--     * A __smaller spoke count__ will result in __more insert contention__ at each spoke and will require
--       __less memory__ to store the timer wheel.
--
-- * The /resolution/ determines both the duration of time that each spoke corresponds to, and how often the timeout
--   thread wakes. For example, with a resolution of __@1s@__, a timer that expires at __@2.5s@__ will not fire until
--   the timeout thread wakes at __@3s@__.
--
--     * A __larger resolution__ will result in __more insert contention__ at each spoke, __less accurate__ timers, and
--       will require __fewer wakeups__ by the timeout thread.
--
--     * A __smaller resolution__ will result in __less insert contention__ at each spoke, __more accurate__ timers, and
--       will require __more wakeups__ by the timeout thread.
--
-- * The timeout thread has some important properties:
--
--     * There is only one, and it fires expired timers synchronously. If your timer actions execute quicky, 'register'
--       them directly. Otherwise, consider registering an action that enqueues the /real/ action to be performed on a
--       job queue.
--
--     * Synchronous exceptions thrown by enqueued @IO@ actions will bring the thread down, and the exception will be
--       propagated to the thread that created the timer wheel. If you want to catch exceptions and log them, for
--       example, you will have to bake this into the registered actions yourself.
--
-- As an example, below is a depiction of a timer wheel with @6@ timers inserted across @8@ spokes, and a resolution of
-- @.1s@. It depicts a cursor at @.3s@, which indicates where the timeout thread currently is.
--
-- @
--  0       .1      .2      .3      .4      .5      .6      .7
-- ┌───────┬───────┬───────┬───────┬───────┬───────┬───────┬───────┐
-- │       │ A⁰    │       │ B¹ C⁰ │ D⁰    │       │       │ E² F⁰ │
-- └───────┴───────┴───────┴───────┴───────┴───────┴───────┴───────┘
--                           ↑
-- @
--
-- After @.1s@, the timeout thread will advance to the next spoke and process all of the timers it passed over. In
-- this case, __C__ will fire, and __B__ will be put back with its count decremented to @0@. This is how the timer wheel
-- can schedule a timer to fire arbitrarily far in the future: its count is simply the number of times its delay wraps
-- the entire duration of the timer wheel.
--
-- @
--  0       .1      .2      .3      .4      .5      .6      .7
-- ┌───────┬───────┬───────┬───────┬───────┬───────┬───────┬───────┐
-- │       │ A⁰    │       │ B⁰    │ D⁰    │       │       │ E² F⁰ │
-- └───────┴───────┴───────┴───────┴───────┴───────┴───────┴───────┘
--                                   ↑
-- @
data TimerWheel = TimerWheel
  { -- | A supply of unique ints.
    supply :: Supply,
    -- | The array of collections of timers.
    wheel :: Wheel,
    thread :: ThreadId
  }

-- | The timeout thread died.
newtype TimerWheelDied
  = TimerWheelDied SomeException
  deriving stock (Show)

instance Exception TimerWheelDied where
  toException = asyncExceptionToException
  fromException = asyncExceptionFromException

-- | Perform an action with a timer wheel.
--
-- /Throws./
--
--   * Calls 'error' if the config is invalid
--   * Throws the exception the given action throws, if any
--   * Throws the exception the timer wheel thread throws, if any
with :: Config -> (TimerWheel -> IO a) -> IO a
with config action =
  case validateConfig config of
    () -> _with config action

_with :: Config -> (TimerWheel -> IO a) -> IO a
_with config action = do
  wheel <- Wheel.create (Config.spokes config) (Micros.fromFixed (Config.resolution config))
  supply <- Supply.new
  parentThread <- myThreadId
  uninterruptibleMask \restore -> do
    thread <-
      forkIOWithUnmask $ \unmask ->
        unmask (Wheel.reap wheel) `catch` \e ->
          case fromException e of
            Just ThreadKilled -> pure ()
            _ -> throwTo parentThread (TimerWheelDied e)
    let cleanup = killThread thread
    let handler :: SomeException -> IO void
        handler ex = do
          cleanup
          case fromException ex of
            Just (TimerWheelDied ex') -> throwIO ex'
            _ -> throwIO ex
    result <- restore (action TimerWheel {supply, wheel, thread}) `catch` handler
    cleanup
    pure result

validateConfig :: Config -> ()
validateConfig config
  | invalid = error ("[timer-wheel] invalid config: " ++ show config)
  | otherwise = ()
  where
    invalid :: Bool
    invalid =
      Config.spokes config <= 0 || Config.resolution config <= 0

-- | @register wheel delay action@ registers an action __@action@__ in timer wheel __@wheel@__ to fire after __@delay@__
-- seconds.
--
-- Returns an action that, when called, attempts to cancel the timer, and returns whether or not it was successful
-- (@False@ means the timer has already fired, or was already cancelled).
--
-- /Throws/.
--
--   * Calls 'error' if the given number of seconds is negative.
register ::
  -- |
  TimerWheel ->
  -- | Delay, in seconds
  Fixed E6 ->
  -- | Action
  IO () ->
  IO (IO Bool)
register wheel (Micros.fromSeconds -> delay) =
  _register wheel delay

-- | Like 'register', but for when you don't intend to cancel the timer.
--
-- /Throws/.
--
--   * Calls 'error' if the given number of seconds is negative.
register_ ::
  -- |
  TimerWheel ->
  -- | Delay, in seconds
  Fixed E6 ->
  -- | Action
  IO () ->
  IO ()
register_ wheel delay action =
  void (register wheel delay action)

_register :: TimerWheel -> Micros -> IO () -> IO (IO Bool)
_register TimerWheel {supply, wheel} delay action = do
  key <- Supply.next supply
  Wheel.insert wheel key delay action

-- | @recurring wheel action delay@ registers an action __@action@__ in timer wheel __@wheel@__ to fire every
-- __@delay@__ seconds (or every /resolution/ seconds, whichever is smaller).
--
-- Returns an action that, when called, cancels the recurring timer.
--
-- /Throws/.
--
--   * Calls 'error' if the given number of seconds is negative.
recurring ::
  TimerWheel ->
  -- | Delay, in seconds
  Fixed E6 ->
  -- | Action
  IO () ->
  IO (IO ())
recurring wheel (Micros.fromSeconds -> delay) action = mdo
  let doAction :: IO ()
      doAction = do
        writeIORef cancelRef =<< _reregister wheel delay doAction
        action
  cancel <- _register wheel delay doAction
  cancelRef <- newIORef cancel
  pure (untilTrue (join (readIORef cancelRef)))
  where
    -- Repeat an IO action until it returns 'True'.
    untilTrue :: IO Bool -> IO ()
    untilTrue m =
      fix \again ->
        m >>= bool again (pure ())

-- | Like 'recurring', but for when you don't intend to cancel the timer.
--
-- /Throws/.
--
--   * Calls 'error' if the given number of seconds is negative.
recurring_ ::
  TimerWheel ->
  -- | Delay, in seconds
  Fixed E6 ->
  -- | Action
  IO () ->
  IO ()
recurring_ wheel (Micros.fromSeconds -> delay) action =
  void (_register wheel delay doAction)
  where
    doAction :: IO ()
    doAction = do
      _ <- _reregister wheel delay doAction
      action

-- Re-register one bucket early, to account for the fact that timers are
-- expired at the *end* of a bucket.
--
-- +---+---+---+---+
-- { A |   |   |   }
-- +---+---+---+---+
--      |
--      The reaper thread fires 'A' approximately here, so if it's meant
--      to be repeated every two buckets, and we just re-register it at
--      this time, three buckets will pass before it's run again. So, we
--      act as if it's still "one bucket ago" at the moment we re-register
--      it.
_reregister :: TimerWheel -> Micros -> IO () -> IO (IO Bool)
_reregister wheel delay =
  _register wheel (if reso > delay then Micros 0 else delay `Micros.minus` reso)
  where
    reso :: Micros
    reso =
      resolution wheel

resolution :: TimerWheel -> Micros
resolution =
  Wheel.resolution . wheel
