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
    , glsSLECmdQueue
    , newGlobalState
    , nextADCount
    ) where


import           RIO                     hiding ( to
                                                , view
                                                , lens
                                                , (^.)
                                                )
import qualified RIO.HashMap                   as HM

import           Control.Lens

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
import           Data.PUS.PUSState              ( defaultPUSState
                                                , nextADCnt
                                                )
import           Data.PUS.Statistics
import           Data.PUS.TCRequest             ( TCRequest )

--import           General.PUSTypes
import           General.Time

import           Verification.Commands

import           Persistence.DBQuery
import           Persistence.DbBackend         as DB

import           Protocol.ProtocolSLE

import           Control.PUS.Classes
--import           GHC.Compact




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
    , glsSLECmdQueue       :: TBQueue SLECommand
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
newGlobalState cfg missionSpecific logErr raiseEventF eventFlags dbBackend queryQueue
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
        frameStat  <- newTVarIO initialStatistics
        packetStat <- newTVarIO initialStatistics
        sleQueue   <- newTBQueueIO 10

        let eventCfg = createEventConfig eventFlags
            eventFn  = if eventCfg ^. cfgEvAll
                then raiseEventF
                else filteredRaiseEvent eventCfg raiseEventF

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
                                , glsSLECmdQueue       = sleQueue
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


instance HasConfig GlobalState where
    getConfig = to glsConfig

instance HasPUSState GlobalState where
    appStateG = to glsState

instance HasMissionSpecific GlobalState where
    getMissionSpecific = to glsMissionSpecific

instance HasFOPState GlobalState where
    copStateG = to glsFOP1
    fopStateG vcid env = HM.lookup vcid (glsFOP1 env)

instance HasCorrelationState GlobalState where
    corrStateG = to glsCorrState

instance HasDataModel GlobalState where
    getDataModelVar = to glsDataModel

instance HasRaiseEvent GlobalState where
    appRaiseEvent = glsRaiseEvent

instance HasTCRqstQueue GlobalState where
    getRqstQueue = to glsTCRequestQueue

instance HasVerif GlobalState where
    registerRequest env rqst pktID ssc = atomically $ writeTBQueue
        (glsVerifCommandQueue env)
        (RegisterRequest rqst pktID ssc)
    requestReleased env rqstID releaseTime status = atomically $ writeTBQueue
        (glsVerifCommandQueue env)
        (SetVerifR rqstID releaseTime status)
    requestVerifyG env rqstID status = atomically
        $ writeTBQueue (glsVerifCommandQueue env) (SetVerifG rqstID status)
    requestVerifyT env rqstID status = atomically
        $ writeTBQueue (glsVerifCommandQueue env) (SetVerifT rqstID status)
    requestVerifyO env rqstID status = atomically
        $ writeTBQueue (glsVerifCommandQueue env) (SetVerifO rqstID status)
    requestVerifyGT env rqstID status = atomically
        $ writeTBQueue (glsVerifCommandQueue env) (SetVerifGT rqstID status)
    requestVerifyGTCnC env (pktID, seqC) status = atomically $ writeTBQueue
        (glsVerifCommandQueue env)
        (SerVerifGTCnC pktID seqC status)
    requestVerifyTMA env (pktID, seqC) status = atomically $ writeTBQueue
        (glsVerifCommandQueue env)
        (SetVerifA pktID seqC status)
    requestVerifyTMS env (pktID, seqC) status = atomically $ writeTBQueue
        (glsVerifCommandQueue env)
        (SetVerifS pktID seqC status)
    requestVerifyTMC env (pktID, seqC) status = atomically $ writeTBQueue
        (glsVerifCommandQueue env)
        (SetVerifC pktID seqC status)
    requestVerifyProgressTM env (pktID, seqC, idx) status =
        atomically $ writeTBQueue
            (glsVerifCommandQueue env)
            (SetVerifP (fromIntegral idx) pktID seqC status)


instance HasDatabase GlobalState where
    getDbBackend env = glsDatabase env
    storeTMFrame env frame =
        maybe (return ()) (`DB.storeTMFrame` frame) (getDbBackend env)
    storeTMFrames env frames =
        maybe (return ()) (`DB.storeTMFrames` frames) (getDbBackend env)
    storePUSPacket env pkt =
        maybe (return ()) (`DB.storePUSPacket` pkt) (getDbBackend env)
    storeTMPacket env pkt =
        maybe (return ()) (`DB.storeTMPacket` pkt) (getDbBackend env)

    queryDB env query = do
        case glsQueryQueue env of
            Just queue -> atomically $ writeTBQueue queue query
            Nothing    -> return ()

instance HasStats GlobalState where
    getFrameStats  = glsFrameStatistics
    getPacketStats = glsPacketStatistics

instance HasTerminate GlobalState where
    appTerminate env = do
        atomically $ writeTBQueue (glsSLECmdQueue env) SLETerminate

instance HasSLE GlobalState where 
    getSleCmdQueue = glsSLECmdQueue
    appSendSLE env cmd = do 
        atomically $ writeTBQueue (glsSLECmdQueue env) cmd 


instance HasGlobalState GlobalState

