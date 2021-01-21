{-# LANGUAGE BlockArguments, DeriveAnyClass, DerivingStrategies, FlexibleInstances, MultiParamTypeClasses #-}

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.IORef
import Data.Maybe (isJust)
import Data.TimerWheel
import System.Mem (performGC)
import System.Mem.Weak (deRefWeak)

main :: IO ()
main = do
  do
    putStrLn "Timer wheel runs scheduled actions"
    with Config { spokes = 4, resolution = 0.05 } \wheel -> do
      var <- newEmptyMVar
      let n = 1000
      replicateM_ n (register_ wheel 0 (putMVar var ()))
      replicateM_ n (takeMVar var)

  do
    putStrLn "Timers can be canceled"
    var <- newEmptyMVar
    with Config { spokes = 4, resolution = 0.05 } \wheel -> do
      let n = 1000
      cancels <- replicateM n (register wheel 0 (putMVar var ()))
      successes <- sequence (take (n `div` 2) cancels)
      replicateM_ (n - length (filter id successes)) (takeMVar var)

  do
    putStrLn "Successful `cancel` returns True (then False)"
    with Config { spokes = 4, resolution = 0.05 } \wheel -> do
      cancel <- register wheel 1 (pure ())
      cancel `is` True
      cancel `is` False

  do
    putStrLn "Unsuccessful `cancel` returns False"
    with Config { spokes = 4, resolution = 0.05 } \wheel -> do
      var <- newEmptyMVar
      cancel <- register wheel 0 (putMVar var ())
      takeMVar var
      cancel `is` False

  do
    putStrLn "Recurring timers work"
    with Config { spokes = 4, resolution = 0.05 } \wheel -> do
      canary <- newIORef () -- kept alive only by timer
      weakCanary <- mkWeakIORef canary (pure ())
      var <- newEmptyMVar
      cancel <- recurring wheel 0 (readIORef canary >> putMVar var ())
      replicateM_ 2 (takeMVar var)
      cancel -- should drop reference canary after a GC
      performGC
      (isJust <$> deRefWeak weakCanary) `is` False

  do
    putStrLn "`with` re-throws exception from background thread"
    catch
      (with Config { spokes = 4, resolution = 0.05 } \wheel -> do
        var <- newEmptyMVar
        register_ wheel 0 (throwIO Bye >> putMVar var ())
        takeMVar var
        throwIO (userError "fail"))
      (\ex ->
        case fromException ex of
          Just Bye -> pure ()
          _ -> throwIO ex)

data Bye = Bye
  deriving stock (Show)
  deriving anyclass (Exception)

class Assert a b where
  is :: a -> b -> IO ()

instance (Eq a, Show a) => Assert (IO a) a where
  is mx y = do
    x <- mx
    unless (x == y) (throwIO (userError (show x ++ " /= " ++ show y)))
