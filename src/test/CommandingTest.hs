{-# LANGUAGE OverloadedStrings
    , BangPatterns
    , GeneralizedNewtypeDeriving
    , DeriveGeneric
    , RecordWildCards
    , FlexibleInstances
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

import Control.PUS.Classes

import Data.PUS.TCTransferFrame
import Data.PUS.TCTransferFrameEncoder
import Data.PUS.CLTU
import Data.PUS.CLTUEncoder
import Data.PUS.GlobalState
import Data.PUS.Config

import Protocol.NCTRS


transferFrames :: [TCTransferFrame]
transferFrames = []


-- class Monad m => MonadConfig m where
--     getConfig :: m (Config)


-- class Monad m => MonadPUSState m where
--     getPUSState :: m PUSState
--     withPUSState :: (PUSState -> PUSState, a) -> m a
--     withPUSState_ :: (PUSState -> PUSState) -> m ()
--     nextADCount :: m Word8


-- class (Monad m, MonadConfig m, MonadPUSState m) => MonadGlobalState m where
--     getGlobalState :: m (GlobalState m)



main :: IO ()
main = do
    state <- newGlobalState defaultConfig T.putStrLn (\ev -> T.putStrLn ("Event: " <> T.pack (show ev)))


    runRIO state $ do

        let chain :: (MonadGlobalState m) => ConduitT () ByteString m ()
            chain = sourceList transferFrames .| tcFrameEncodeC .| tcFrameToCltuC .| cltuEncodeC .| cltuToNcduC .| encodeTcNcduC

        runGeneralTCPClient (clientSettings 10000 "localhost") $ \app->
            void $ concurrently
                (runConduitRes (chain .| appSink app))
                (runConduitRes (appSource app .| stdoutC))