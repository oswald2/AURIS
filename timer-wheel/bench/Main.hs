{-# language LambdaCase          #-}
{-# language ScopedTypeVariables #-}

import Control.Concurrent.Async (replicateConcurrently_)
import Control.Monad
import Data.Fixed
import Data.Foldable
import Data.IORef
import GHC.Event (getSystemTimerManager, registerTimeout, unregisterTimeout)
import System.Environment
import System.IO.Unsafe
import System.Random

import qualified Data.TimerWheel.List as TimerWheel.List
import qualified Data.TimerWheel.PSQ as TimerWheel.PSQ
import qualified Data.TimerWheel.SortedList as TimerWheel.SortedList

main :: IO ()
main =
  getArgs >>= \case
    ["wheel-list", which, s, t, n, m] ->
      timerWheelMain
        (which == "insert")
        (read s)
        (read t)
        (read n)
        (read m)
        TimerWheel.List.new
        TimerWheel.List.register
    ["wheel-sorted-list", which, s, t, n, m] ->
      timerWheelMain
        (which == "insert")
        (read s)
        (read t)
        (read n)
        (read m)
        TimerWheel.SortedList.new
        TimerWheel.SortedList.register
    ["wheel-psq", which, s, t, n, m] ->
      timerWheelMain
        (which == "insert")
        (read s)
        (read t)
        (read n)
        (read m)
        TimerWheel.PSQ.new
        TimerWheel.PSQ.register
    ["ghc", which, n, m] ->
      ghcMain (which == "insert") (read n) (read m)
    _ ->
      traverse_ putStrLn
        [ "Usage:"
        , ""
        , "cabal new-run bench -- wheel-list        <which?> B S N M"
        , "cabal new-run bench -- wheel-list        <which?> B S N M"
        , "cabal new-run bench -- wheel-sorted-list <which?> B S N M"
        , "cabal new-run bench -- wheel-sorted-list <which?> B S N M"
        , "cabal new-run bench -- wheel-psq         <which?> B S N M"
        , "cabal new-run bench -- wheel-psq         <which?> B S N M"
        , "cabal new-run bench -- ghc               <which?>     N M"
        , ""
        , "<which?> ('insert' or 'remove')   Kind of benchmark to run"
        , "B        (int)                    Number of buckets in the timer wheel"
        , "S        (float)                  Accuracy of the timer wheel (seconds)"
        , "N        (int)                    How many threads to insert simultaneously"
        , "M        (int)                    How many timers each thread should insert"
        ]


timerWheelMain
  :: Bool
  -> Int
  -> Fixed E6
  -> Int
  -> Int
  -> (Int -> Fixed E6 -> IO wheel)
  -> (Int -> IO () -> wheel -> IO (IO Bool))
  -> IO ()
timerWheelMain which s t n m new register = do
  wheel <-
    new s t

  replicateConcurrently_ n $ do
    timers <-
      replicateM m $ do
        x:xs <- readIORef intsRef
        writeIORef intsRef xs
        register x (pure ()) wheel
    unless which (for_ timers id)

ghcMain :: Bool -> Int -> Int -> IO ()
ghcMain which n m = do
  mgr <- getSystemTimerManager

  replicateConcurrently_ n $ do
    timers <-
      replicateM m $ do
        x:xs <- readIORef intsRef
        writeIORef intsRef xs
        registerTimeout mgr x (pure ())
    unless which (for_ timers (unregisterTimeout mgr))

intsRef :: IORef [Int]
intsRef =
  unsafePerformIO (newIORef (randomRs (1*1000*1000, 10*1000*1000) (mkStdGen 1)))
{-# NOINLINE intsRef #-}
