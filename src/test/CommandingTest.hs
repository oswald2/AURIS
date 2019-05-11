{-# LANGUAGE OverloadedStrings
    , BangPatterns
    , GeneralizedNewtypeDeriving
    , DeriveGeneric
    , RecordWildCards
#-}
module Main
where


import RIO
import qualified RIO.Text as T
import qualified Data.Text.IO as T

import Conduit
import Data.Conduit
import Data.Conduit.List
import Data.Conduit.Network

import UnliftIO.Async

import Control.PUS.Monads

import Data.PUS.TCTransferFrame
import Data.PUS.TCTransferFrameEncoder
import Data.PUS.CLTU
import Data.PUS.CLTUEncoder
import Data.PUS.GlobalState
import Data.PUS.Config

import Protocol.NCTRS


transferFrames :: [TCTransferFrame]
transferFrames = []



main :: IO ()
main = do
    state <- newGlobalState defaultConfig T.putStrLn (\ev -> T.putStrLn ("Event: " <> T.pack (show ev)))


    runRIO state $ do

        let chain :: (MonadGlobalState m) => ConduitT () ByteString m ()
            chain = sourceList transferFrames .| tcFrameEncodeC .| tcFrameToCltuC .| cltuEncodeC .| cltuToNcduC .| encodeTcNcduC

        runGeneralTCPClient (clientSettings 10000 "localhost") $ \app-> _
            -- void $ concurrently
            --     (appSink app)
            --     (appSource app .| stdoutC)