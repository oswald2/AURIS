{-# LANGUAGE OverloadedStrings
    , BangPatterns
    , GeneralizedNewtypeDeriving
    , DeriveGeneric
    , RecordWildCards
    , FlexibleInstances
    , BinaryLiterals
    , FlexibleContexts
    , NumericUnderscores
#-}
module Main where

import           RIO
import qualified RIO.Text                      as T
import qualified Data.Text.IO                  as T
-- import qualified RIO.HashMap                   as HM
import qualified RIO.Vector                    as V
import qualified RIO.Vector.Partial            as V
import           Data.Text.Short                ( ShortText )
import qualified Data.HashTable.ST.Basic       as HT

import           Data.MIB.LoadMIB

import           System.Environment
import           System.Random.MWC

import           GHC.Conc.Sync                  ( getNumProcessors
                                                , setNumCapabilities
                                                )

import           Data.PUS.GlobalState
import           Data.PUS.Config
import           Data.PUS.MissionSpecific.Definitions

import           Data.TM.Parameter
import           Data.TM.Value
import           Data.TM.Validity
--import           Data.TM.TMParameterDef
import           General.Time

import           Data.DataModel



genRandomValue :: PrimMonad m => Gen (PrimState m) -> m TMValue
genRandomValue gen = do
    v <- uniformR (0, 255) gen
    return $ TMValue (TMValUInt v) clearValidity

genRandomParameter
    :: (PrimMonad m, MonadIO m, V.Vector v ShortText)
    => Gen (PrimState m)
    -> v ShortText
    -> m TMParameter
genRandomParameter gen namevec = do
    val <- genRandomValue gen
    t   <- liftIO getCurrentTime
    let len = V.length namevec

    idx <- uniformR (0, len - 1) gen

    let name = namevec V.! idx

    return $ TMParameter name t val Nothing


nameVec :: DataModel -> Vector ShortText
nameVec model =
    let lst = HT.toList $ _dmParameters model
    in V.fromList . map fst $ lst


feedParamValues
    :: (PrimMonad m, MonadIO m)
    => Gen (PrimState m)
    -> DataModel
    -> TBQueue TMParameter
    -> m ()
feedParamValues gen model queue = do
    let nv = nameVec model

    replicateM_ 2000 $ do
        genRandomParameter gen nv >>= liftIO . atomically . writeTBQueue queue
        del <- uniformR (0, 100_000) gen
        threadDelay del



main :: IO ()
main = do
    np <- getNumProcessors
    setNumCapabilities np

    [path]        <- getArgs

    queue         <- newTBQueueIO 1000
    gen           <- create

    defLogOptions <- logOptionsHandle stdout True
    let logOptions = setLogMinLevel LevelError defLogOptions
    withLogFunc logOptions $ \logFunc -> do
        state <- newGlobalState
            defaultConfig
            (defaultMissionSpecific defaultConfig)
            logFunc
            (\ev -> T.putStrLn ("Event: " <> T.pack (show ev)))

        runRIO state $ do
            dataModel <- loadMIB path
            case dataModel of
                Left err -> do
                    liftIO $ T.putStrLn err
                    exitFailure
                Right model -> do
                  -- TODO: setup the FRP model
                    _ <- async $ feedParamValues gen model queue
                    return ()


