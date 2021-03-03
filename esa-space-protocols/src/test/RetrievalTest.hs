{-# LANGUAGE OverloadedStrings
    , BangPatterns
    , GeneralizedNewtypeDeriving
    , DeriveGeneric
    , RecordWildCards
    , FlexibleInstances
    , BinaryLiterals
    , TemplateHaskell
#-}
module Main where


import           RIO
import qualified RIO.Text                      as T
import qualified Data.Text.IO                  as T

import           Conduit
import           Data.Conduit.List
import qualified Data.Conduit.Combinators      as C
import           Data.Conduit.Network

import           Data.PUS.TCTransferFrame
import           Data.PUS.TCTransferFrameEncoder
                                                ( tcFrameToCltuC
                                                , tcSegmentToTransferFrame
                                                )
import           Data.PUS.CLTU
import           Data.PUS.CLTUEncoder
import           Data.PUS.GlobalState
import           Data.PUS.Config
import           General.PUSTypes
import           Data.PUS.PUSPacketEncoder
import           Data.PUS.SegmentEncoder
import           General.APID
import           Data.PUS.TCRequest
import           Data.PUS.TCPacketEncoder
import           Data.PUS.TCPacket
import           Data.PUS.Parameter
--import           Data.PUS.Value
import           Data.PUS.MissionSpecific.Default
--import           Data.PUS.Counter
import           Data.PUS.Events                ( EventFlag(..) )

--import           Protocol.NCTRSProcessor
--import           Protocol.ProtocolInterfaces

import           GHC.Conc.Sync

--import           Verification.Verification

--import           Refined

--import           Control.PUS.Classes
import           Data.Mongo.Processing
import           Data.DbConfig.MongoDB

import           Persistence.DbBackend


logF :: LogSource -> LogLevel -> Utf8Builder -> IO ()
logF source level builder = do
    T.putStrLn
        $  source
        <> ": "
        <> T.pack (show level)
        <> ": "
        <> utf8BuilderToText builder


main :: IO ()
main = do
    np <- getNumProcessors
    setNumCapabilities np

    defLogOptions <- logOptionsHandle stdout True
    let logOptions = setLogMinLevel LevelError defLogOptions
    withLogFunc logOptions $ \logFunc -> do
        dbState   <- newDbState logFunc
        dbBackend <- runRIO dbState $ startDbStoreThreads defaultMongoDBConfig
        queryQueue <- newTBQueueIO 200
        state     <- newGlobalState
            defaultConfig
            (defaultMissionSpecific defaultConfig)
            logFunc
            (\ev -> T.putStrLn ("Event: " <> T.pack (show ev)))
            [EVFlagAll]
            (Just dbBackend)
            queryQueue

        runRIO state $ do
            let resultFunc = \result -> liftIO $ T.putStrLn ("Result: " <> T.pack (show result))
            queue <- newTBQueueIO 500

            startDbQueryThreads defaultMongoDBConfig dbBackend resultFunc queue

            --env    <- ask
            frames <- allTMFrames defaultMongoDBConfig
            liftIO
                $  T.putStrLn
                $  "Received Frames from DB:\n"
                <> T.pack (show (length frames))
                <> " rows, last row:\n"
                <> T.pack (show (last frames))
