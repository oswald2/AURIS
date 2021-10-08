{-|
Module      : Data.PUS.GlobalState
Description : Represents the global state of the encoders
Copyright   : (c) Michael Oswald, 2019
License     : BSD-3
Maintainer  : michael.oswald@onikudaki.net
Stability   : experimental
Portability : POSIX

Contains the 'AppState' and 'GlobalState' types which encapsulate the
complete state of the used RIO monad. The 'GlobalState' consists of
the PUS Config and several TVars, which contain the transient application
state (or library state)
|-}
{-# LANGUAGE OverloadedStrings
    , BangPatterns
    , GeneralizedNewtypeDeriving
    , DeriveGeneric
    , RecordWildCards
    , NoImplicitPrelude
    , FlexibleInstances
    , MultiParamTypeClasses
#-}
module Data.PUS.GlobalState
    ( GlobalState
    , AppState
    , FOP1State
    , COP1State
    , CorrelationVar
    , glsConfig
    , glsState
    , glsCorrState
    , glsFOP1
    , glsLogFunc
    , glsRaiseEvent
    , glsMissionSpecific
    , glsDataModel
    , glsTCRequestQueue
    , glsVerifCommandQueue
    , glsDatabase
    , glsQueryQueue
    , glsFrameStatistics 
    , glsPacketStatistics
    , newGlobalState
    , nextADCount
    ) where


import           RIO                     hiding ( to
                                                , view
                                                )
import qualified RIO.HashMap                   as HM

import           UnliftIO.STM                   ( )

import           Data.DataModel                 ( DataModel
                                                , empty
                                                )

import           Data.PUS.COP1Types
import           Data.PUS.Config
import           Data.PUS.EventHandler          ( cfgEvAll
                                                , createEventConfig
                                                , filteredRaiseEvent
                                                )
import           Data.PUS.Events                ( Event
                                                , EventFlag
                                                )
import           Data.PUS.MissionSpecific.Definitions
                                                ( PUSMissionSpecific )
import           Data.PUS.PUSState              ( PUSState
                                                , defaultPUSState
                                                , nextADCnt
                                                )
import           Data.PUS.TCRequest             ( TCRequest )

import           General.PUSTypes
import           General.Time

import           Verification.Commands

import           Data.PUS.Statistics
import           Persistence.DBQuery
import           Persistence.DbBackend

--import           GHC.Compact


-- | The AppState is just a type alias
type AppState = TVar PUSState

-- | Stores the current correlation coefficient
type CorrelationVar = TVar CorrelationCoefficients

-- | The state of the FOP1 machine
type FOP1State = TVar FOPState

type COP1State = HashMap VCID FOP1State


rqstQueueSize :: Natural
rqstQueueSize = 1000

-- | The 'GlobalState' contains the configuration, several TVars to
-- transient state and some functions which must be provided by the
-- user of the library. Currently for logging and raising events
data GlobalState = GlobalState
    { glsConfig            :: !Config
    , glsState             :: !AppState
    , glsFOP1              :: COP1State
    , glsCorrState         :: CorrelationVar
    , glsDataModel         :: TVar DataModel
    , glsMissionSpecific   :: PUSMissionSpecific
    , glsRaiseEvent        :: Event -> IO ()
    , glsLogFunc           :: !LogFunc
    , glsTCRequestQueue    :: TBQueue [TCRequest]
    , glsVerifCommandQueue :: TBQueue VerifCommand
    , glsDatabase          :: Maybe DbBackend
    , glsQueryQueue        :: Maybe (TBQueue DBQuery)
    , glsFrameStatistics   :: TVar Statistics
    , glsPacketStatistics  :: TVar Statistics
    }

-- | Constructor for the global state. Takes a configuration, a
-- logging function as specified by the RIO library and a raiseEvent
-- function to report events to the application
newGlobalState
    :: Config
    -> PUSMissionSpecific
    -> LogFunc
    -> (Event -> IO ())
    -> [EventFlag]
    -> Maybe DbBackend
    -> Maybe (TBQueue DBQuery)
    -> IO GlobalState
newGlobalState cfg missionSpecific logErr raiseEvent eventFlags dbBackend queryQueue
    = do
        st     <- defaultPUSState cfg
        tv     <- newTVarIO st
        cv     <- newTVarIO defaultCoeffs
        dmodel <- newTVarIO Data.DataModel.empty
        let vcids = cfgVCIDs cfg
        fopTVars <- mapM (newTVarIO . initialFOPState) vcids
        let fop1 = HM.fromList $ zip vcids fopTVars
        rqstQueue  <- newTBQueueIO rqstQueueSize
        verifQueue <- newTBQueueIO 5000
        frameStat <- newTVarIO initialStatistics
        packetStat <- newTVarIO initialStatistics

        let eventCfg = createEventConfig eventFlags
            eventFn  = if eventCfg ^. cfgEvAll
                then raiseEvent
                else filteredRaiseEvent eventCfg raiseEvent

        let state = GlobalState { glsConfig            = cfg
                                , glsState             = tv
                                , glsCorrState         = cv
                                , glsFOP1              = fop1
                                , glsRaiseEvent        = eventFn
                                , glsLogFunc           = logErr
                                , glsDataModel         = dmodel
                                , glsMissionSpecific   = missionSpecific
                                , glsTCRequestQueue    = rqstQueue
                                , glsVerifCommandQueue = verifQueue
                                , glsDatabase          = dbBackend
                                , glsQueryQueue        = queryQueue
                                , glsFrameStatistics   = frameStat
                                , glsPacketStatistics  = packetStat
                                }
        pure state

-- | returns the next counter value for TC transfer frames
-- in AD transmission mode
nextADCount :: AppState -> STM Word8
nextADCount st = do
    state <- readTVar st
    let (newSt, cnt) = nextADCnt state
    writeTVar st newSt
    pure cnt



-- | Instance of the logging function for the global state
instance HasLogFunc GlobalState where
    logFuncL = lens glsLogFunc (\c lf -> c { glsLogFunc = lf })


