{-# LANGUAGE NoImplicitPrelude, NumericUnderscores #-}
module Main where


import           RIO

import           Conduit.TimedBuffer

import           System.IO                      



main :: IO ()
main = do
    buf <- newTimedBufferIO 3

    race_ (writeThread buf) (readThread buf)

    putStrLn "Done."
    return ()

readThread :: TimedBuffer Int -> IO ()
readThread buf = do
    res <- readTimedBuffer 2000000 buf
    putStrLn $ "Read: " <> show res
    readThread buf


writeThread :: TimedBuffer Int -> IO ()
writeThread buf = do
    putStrLn "Writing 4 values..."
    forM_ [1 .. 4] $ \x -> writeTimedBuffer buf x
    putStrLn "Waiting..."
    threadDelay 4_000_000
    putStrLn "Writing 10 values..."
    forM_ [1 .. 10] $ \x -> writeTimedBuffer buf x
    putStrLn "Waiting..."
    threadDelay 4_000_000
    putStrLn "Writing 100 values..."
    forM_ [1..100] $ \x -> writeTimedBuffer buf x
    putStrLn "Waiting..."
    threadDelay 4_000_000
