{-|
Module      : Interface.Interface
Description : Central data type and functions for interacting with the MCS
Copyright   : (c) Michael Oswald, 2019
License     : BSD-3
Maintainer  : michael.oswald@onikudaki.net
Stability   : experimental
Portability : POSIX

This module contains the central data type of the interface. It consists
of a table of functions (actions) which need to be wired to the functions
in the used libraries (e.g. esa-space-protocols). This is the direction 
of actions/activities. 

The other directions happens via events. The 'Interface' also contains
a 'raiseEvent' function which is used to inform the external clients
about things happening in the MCS.

The 'Interface' can be used locally only (within one process) directly 
with the 'Interface' data type. Also available is a socket based interface
(see 'Interface.SocketInterface'), which must be instantiated on startup 
and added to the 'Interface' via the 'addInterface' function. The socket
interface can communicate with a client via either Strings, Binary or JSON.
-}
{-# LANGUAGE OverloadedStrings
    , BangPatterns
    , NoImplicitPrelude

#-}
module Interface.Interface
    ( Interface
    , ActionTable(..)
    , EventHandler
    , createInterface
    , addInterface
    , callInterface
    , syncCallInterface
    , ifRaiseEvent
    ) where


import           Data.PUS.TCRequest
import           Data.TC.TCDef
import           Data.Text.Short                ( ShortText )
import           RIO
import qualified RIO.Vector                    as V

import           Interface.CoreProcessor
import           Interface.Events

import           Persistence.DBQuery            ( DBQuery )

import           General.PUSTypes

-- | Table of actions which can be called. Direction is from the 
-- client (GUI, script, command line) to the MCS
data ActionTable = ActionTable
    { actionQuit             :: IO ()
    , actionImportMIB        :: FilePath -> FilePath -> IO ()
    , actionLogMessage       :: LogSource -> LogLevel -> Utf8Builder -> IO ()
    , actionSendTCRequest    :: TCRequest -> IO ()
    , actionSendTCGroup      :: [TCRequest] -> IO ()
    , actionQueryDB          :: DBQuery -> IO ()
    , actionGetTCSync :: TCDef -> ShortText -> TransmissionMode -> IO TCRequest
    , actionStatResetFrames  :: IO ()
    , actionStatResetPackets :: IO ()
    , actionBindRAF          :: Text -> IO ()
    , actionUnbindRAF        :: Text -> IO ()
    , actionStartRAF         :: Text -> IO ()
    , actionStopRAF          :: Text -> IO ()
    , actionBindCLTU         :: Text -> IO ()
    , actionUnbindCLTU       :: Text -> IO ()
    , actionStartCLTU        :: Text -> IO ()
    , actionStopCLTU         :: Text -> IO ()
    }

-- | Data type for the event handler.
type EventHandler = IfEvent -> IO ()

-- | The interface data type itself. Contains basically just the 
-- action table and the event handler. There can be multiple 
-- event handlers installed if more interfaces are used.
data Interface = Interface
    { ifActionTable :: ActionTable
    , ifEventFuncs  :: Vector EventHandler
    }

callAction :: TBQueue InterfaceAction -> InterfaceAction -> IO ()
callAction queue action = atomically $ writeTBQueue queue action



actionTable
    :: TBQueue InterfaceAction -> Maybe (TBQueue DBQuery) -> ActionTable
actionTable queue (Just queryQueue) = (actionTable queue Nothing)
    { actionQueryDB = atomically . writeTBQueue queryQueue
    }
actionTable queue Nothing = ActionTable
    { actionQuit             = callAction queue Quit
    , actionImportMIB        = \p s -> callAction queue (ImportMIB p s)
    , actionLogMessage       = \s l msg -> callAction queue (LogMsg s l msg)
    , actionSendTCRequest    = callAction queue . SendTCRequest
    , actionSendTCGroup      = callAction queue . SendTCGroup
    , actionQueryDB          = \_ -> pure ()
    , actionGetTCSync        = \tcDef source transMode -> do
                                   var <- newEmptyTMVarIO
                                   callAction
                                       queue
                                       (GetTCSync tcDef source transMode var)
                                   atomically $ takeTMVar var
    , actionStatResetFrames  = callAction queue ResetStatsFrames
    , actionStatResetPackets = callAction queue ResetStatsPackets
    , actionBindRAF          = \sii -> callAction queue (BindRAF sii)
    , actionUnbindRAF        = \sii -> callAction queue (UnbindRAF sii)
    , actionStartRAF         = \sii -> callAction queue (StartRAF sii)
    , actionStopRAF          = \sii -> callAction queue (StopRAF sii)
    , actionBindCLTU         = \sii -> callAction queue (BindCLTU sii)
    , actionUnbindCLTU       = \sii -> callAction queue (UnbindCLTU sii)
    , actionStartCLTU        = \sii -> callAction queue (StartCLTU sii)
    , actionStopCLTU         = \sii -> callAction queue (StopCLTU sii)
    }


-- | creates the 'Interface' from the given 'EventHandler'.
createInterface
    :: EventHandler
    -> Bool
    -> IO (Interface, TBQueue InterfaceAction, Maybe (TBQueue DBQuery))
createInterface handler True = do
    queue      <- newTBQueueIO 5000
    queryQueue <- newTBQueueIO 200
    pure
        ( Interface (actionTable queue (Just queryQueue)) (V.singleton handler)
        , queue
        , Just queryQueue
        )
createInterface handler False = do
    queue <- newTBQueueIO 5000
    pure
        ( Interface (actionTable queue Nothing) (V.singleton handler)
        , queue
        , Nothing
        )

-- | Adds a new event handler to the given 'Interface'. Only an event handler
-- is added, the 'ActionTable' stays the same
addInterface :: Interface -> EventHandler -> Interface
addInterface interface f =
    interface { ifEventFuncs = ifEventFuncs interface `V.snoc` f }

-- | Call a specific action on the 'Interface'. This is the primary function that a local
-- client must use to invoke action in the MCS. The client should not call functions in the 
-- MCS libraries directly.
callInterface :: Interface -> (ActionTable -> t) -> t
callInterface interface f = f (ifActionTable interface)

-- | Like 'callInterface', but, since 'callInterface' is asynchronous, this is a 
-- synchronous version. It must use a synchronous action, which takes a TMVar. 
-- The TMVar should be filled by the action with the result. 'syncCallInterface' 
-- waits until the TMVar is filled with the result and returns the result.
--
-- Note that this is slower and possibly blocking, so this function should only 
-- be used when really needed. It is best to use the asynchronous mechanism and
-- just react to the returned events. 
-- Therefore, not many synchronous actions are provided
syncCallInterface :: Interface -> (ActionTable -> TMVar a -> IO ()) -> IO a
syncCallInterface interface f = do
    mvar <- newEmptyTMVarIO
    f (ifActionTable interface) mvar
    atomically $ takeTMVar mvar

-- | Main function which is used inside the MCS libraries to raise an event.
-- Distributes the event to all registered event handlers (local interfaces,
-- socket interfaces etc)
ifRaiseEvent :: Interface -> IfEvent -> IO ()
ifRaiseEvent interface event = V.mapM_ (\f -> f event) (ifEventFuncs interface)



