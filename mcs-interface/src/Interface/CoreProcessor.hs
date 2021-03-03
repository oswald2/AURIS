module Interface.CoreProcessor
    ( runCoreThread
    , InterfaceAction(..)
    ) where


import           RIO

import           Application.DataModel

import           Control.PUS.Classes

import           Data.PUS.Events
import           Data.PUS.TCRequest

import           Persistence.DBQuery

data InterfaceAction =
  Quit
  | ImportMIB FilePath FilePath
  | LogMsg LogSource LogLevel Utf8Builder
  | SendTCRequest TCRequest
  | SendTCGroup [TCRequest]
  | RequestAllTMFrames
  | QueryDB DBQuery
  deriving (Generic)


runCoreThread
    :: ( MonadUnliftIO m
       , MonadReader env m
       , HasRaiseEvent env
       , HasDataModel env
       , HasLogFunc env
       , HasTCRqstQueue env
       , HasDatabase env
       )
    => TBQueue InterfaceAction
    -> m ()
runCoreThread queue = do
    logDebug "Starting CoreThread..."
    loop
  where
    loop = do
        msg <- atomically $ readTBQueue queue
        case msg of
            Quit -> do
                logInfo "Terminating!"
                return ()
            _ -> do
                processMsg msg
                loop


processMsg
    :: ( MonadUnliftIO m
       , MonadReader env m
       , HasRaiseEvent env
       , HasDataModel env
       , HasLogFunc env
       , HasTCRqstQueue env
       , HasDatabase env
       )
    => InterfaceAction
    -> m ()
processMsg Quit                           = return ()
processMsg (ImportMIB path serializePath) = importMIB path serializePath
processMsg (LogMsg source level msg     ) = logGeneric source level msg
processMsg (SendTCRequest rqst          ) = do
    q <- view getRqstQueue
    atomically $ writeTBQueue q [rqst]
processMsg (SendTCGroup group) = do
    q <- view getRqstQueue
    atomically $ writeTBQueue q group
processMsg (QueryDB query) = do
    env <- ask
    liftIO $ queryDB env query

-- processMsg RequestAllTMFrames = do 
--   env <- ask
--   frames <- liftIO $ allFrames env
--   logInfo $ "Received Frames from DB:\n" <> displayShow frames


importMIB
    :: ( MonadUnliftIO m
       , MonadReader env m
       , HasRaiseEvent env
       , HasDataModel env
       , HasLogFunc env
       )
    => FilePath
    -> FilePath
    -> m ()
importMIB path serializePath = do
    logDebug "Loading data model..."
    env    <- ask
    model' <- loadDataModel (LoadFromMIB path serializePath)
    case model' of
        Left err -> do
            logDebug "Error loading MIB"
            liftIO $ raiseEvent env (EVAlarms (EVMIBLoadError err))
        Right model -> do
            setDataModel env model
            logDebug "Successfully loaded MIB"
            liftIO $ raiseEvent env (EVAlarms EVMIBLoaded)

