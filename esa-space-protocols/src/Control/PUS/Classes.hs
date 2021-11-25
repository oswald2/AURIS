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
    ( AppState
    , FOP1State
    , COP1State
    , CorrelationVar
    , HasConfig(..)
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
    , HasStats(..)
    , HasTerminate(..)
    ) where

import           RIO                     hiding ( (^.)
                                                , to
                                                )
import qualified RIO.HashMap.Partial           as HM

import           Control.Lens.Getter

import           Data.DataModel
import           Data.PUS.COP1Types
import           Data.PUS.Config
import           Data.PUS.Events
import           Data.PUS.ExtractedDU           ( ExtractedDU )
--import           Data.PUS.GlobalState
import           Data.PUS.MissionSpecific.Definitions
                                                ( PUSMissionSpecific )
import           Data.PUS.PUSPacket             ( PUSPacket )
import           Data.PUS.PUSState              ( PUSState )
import           Data.PUS.Statistics
import           Data.PUS.TCRequest
import           Data.PUS.TMFrame               ( TMFrame )
import           Data.PUS.TMPacket              ( TMPacket )
import           Data.PUS.Verification

import           General.PUSTypes
import           General.Time

import           Verification.Commands

import           Persistence.DBQuery            ( DBQuery )
import           Persistence.DbBackend         as DB

--import           GHC.Compact




-- | The AppState is just a type alias
type AppState = TVar PUSState

-- | Stores the current correlation coefficient
type CorrelationVar = TVar CorrelationCoefficients

-- | The state of the FOP1 machine
type FOP1State = TVar FOPState

type COP1State = HashMap VCID FOP1State



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
    fopStateG :: VCID -> env -> Maybe FOP1State


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
    storeTMFrame :: env -> ExtractedDU TMFrame -> IO ()
    storeTMFrames :: env -> [ExtractedDU TMFrame] -> IO ()
    storePUSPacket :: env -> ExtractedDU PUSPacket -> IO ()
    storeTMPacket :: env -> TMPacket -> IO ()

    queryDB :: env -> DBQuery -> IO ()


-- | Class for app statistics
class HasStats a where
  getFrameStats :: a -> TVar Statistics
  getPacketStats :: a -> TVar Statistics

-- | Class for termination of the processign chains
class HasTerminate a where
    terminate :: a -> IO ()

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
    HasDatabase env,
    HasStats env,
    HasTerminate env) => HasGlobalState env



