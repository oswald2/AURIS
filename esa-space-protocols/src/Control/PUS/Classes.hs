{-|
Module      : Control.PUS.Classes
Description : Various classes for config- and state-handling within RIO
Copyright   : (c) Michael Oswald, 2019
License     : BSD-3
Maintainer  : michael.oswald@onikudaki.net
Stability   : experimental
Portability : POSIX

This module contains classes with the HasX pattern to have finer gained
access control to IO functions. Used within the encoding conduits.
|-}
{-# LANGUAGE
    NoImplicitPrelude
    , MultiParamTypeClasses
    , FlexibleInstances
    , RankNTypes
#-}
module Control.PUS.Classes
    ( HasConfig(..)
    , HasPUSState(..)
    , HasGlobalState
    , HasFOPState(..)
    , HasCorrelationState(..)
    , HasMissionSpecific(..)
    , HasDataModel(..)
    , HasTCRqstQueue(..)
    , getDataModel
    , setDataModel
    , HasRaiseEvent(..)
    , HasVerif(..)
    , HasDatabase(..)
    ) where

import           RIO                     hiding ( to
                                                , (^.)
                                                )
import qualified RIO.HashMap.Partial           as HM

import           Control.Lens.Getter

import           Data.DataModel
import           Data.PUS.Config
import           Data.PUS.Events
import           Data.PUS.TMStoreFrame
import           Data.PUS.GlobalState
import           Data.PUS.MissionSpecific.Definitions
                                                ( PUSMissionSpecific )
import           Data.PUS.TCRequest

import           General.PUSTypes
import           General.Time

import           Verification.Commands
import           Verification.Verification

import           Persistence.DbProcessing      as DB
import           Persistence.Conversion.Types
import           Persistence.Conversion.TMFrame ()



-- | This class specifies how to get a configuration
class HasConfig env where
    getConfig :: Getter env Config

-- | Class for getting the application state
class HasPUSState env where
    appStateG :: Getter env AppState

-- | Class for getting the missions specific handlers
class HasMissionSpecific env where
    getMissionSpecific :: Getter env PUSMissionSpecific

-- | Class for getting the FOP1 State
class HasFOPState env where
    copStateG :: Getter env COP1State
    fopStateG :: VCID -> env -> FOP1State


-- | class for getting the time correlation coefficients
class HasCorrelationState env where
    corrStateG :: Getter env CorrelationVar

-- | class for accessing the data model 
class HasDataModel env where
    getDataModelVar :: Getter env (TVar DataModel)

-- | class for injecting TC Requests into the system
class HasTCRqstQueue env where
  getRqstQueue :: Getter env (TBQueue [TCRequest])


getDataModel :: (MonadIO m) => HasDataModel env => env -> m DataModel
getDataModel env = do
    liftIO $ readTVarIO (env ^. getDataModelVar)

setDataModel :: (MonadIO m) => HasDataModel env => env -> DataModel -> m ()
setDataModel env dm = do
    atomically $ writeTVar (env ^. getDataModelVar) dm


-- | class for raising an event to the user interfaces
class HasRaiseEvent env where
    raiseEvent :: env -> Event -> IO ()


-- | Class used for TC verification.
class HasVerif env where
  registerRequest :: env -> TCRequest -> PktID -> SeqControl -> IO ()
  requestReleased :: env -> RequestID -> SunTime -> ReleaseStage -> IO ()
  requestVerifyG :: env -> RequestID -> GroundStage -> IO ()
  requestVerifyT :: env -> RequestID -> GroundStage -> IO ()
  requestVerifyO :: env -> RequestID -> GroundStage -> IO ()
  requestVerifyGT :: env -> RequestID -> GroundStage -> IO ()
  requestVerifyGTCnC :: env -> (PktID, SeqControl) -> GroundStage -> IO ()
  requestVerifyTMA :: env -> (PktID, SeqControl) -> TMStage -> IO ()
  requestVerifyTMS :: env -> (PktID, SeqControl) -> TMStage -> IO ()
  requestVerifyTMC :: env -> (PktID, SeqControl) -> TMStage -> IO ()
  requestVerifyProgressTM :: env -> (PktID, SeqControl, Word8) -> TMStage -> IO ()


-- | This class specifies how to get database path
class HasDatabase env where
    getDbBackend :: env -> Maybe DbBackend
    storeTMFrame :: env -> TMStoreFrame -> IO ()
    storeTMFrames :: env -> [TMStoreFrame] -> IO ()



-- | Class for accessing the global state
class (HasConfig env,
    HasDatabase env,
    HasPUSState env,
    HasFOPState env,
    HasMissionSpecific env,
    HasCorrelationState env,
    HasLogFunc env,
    HasDataModel env,
    HasRaiseEvent env,
    HasTCRqstQueue env,
    HasVerif env,
    HasDatabase env) => HasGlobalState env



instance HasConfig GlobalState where
    getConfig = to glsConfig

instance HasPUSState GlobalState where
    appStateG = to glsState

instance HasMissionSpecific GlobalState where
    getMissionSpecific = to glsMissionSpecific

instance HasFOPState GlobalState where
    copStateG = to glsFOP1
    fopStateG vcid env = glsFOP1 env HM.! vcid

instance HasCorrelationState GlobalState where
    corrStateG = to glsCorrState

instance HasDataModel GlobalState where
    getDataModelVar = to glsDataModel

instance HasRaiseEvent GlobalState where
    raiseEvent = glsRaiseEvent

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
    storeTMFrame env frame = maybe (return ())
                                   (\db -> DB.storeTMFrame db (toDB frame))
                                   (getDbBackend env)
    storeTMFrames env frames =
        maybe (return ())
              (\db -> DB.storeTMFrames db (map toDB frames))
              (getDbBackend env)

instance HasGlobalState GlobalState

