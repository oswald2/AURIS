module Interface.CoreProcessor
  ( runCoreThread
  , InterfaceAction(..)
  )
where


import           RIO

import           Application.DataModel

import           Control.PUS.Classes
import           Data.PUS.Events



data InterfaceAction =
  Quit
  | ImportMIB FilePath FilePath
  deriving (Show, Generic)


runCoreThread
  :: ( MonadUnliftIO m
     , MonadReader env m
     , HasRaiseEvent env
     , HasDataModel env
     , HasLogFunc env
     )
  => TBQueue InterfaceAction
  -> m ()
runCoreThread queue = do 
  logDebug "Starting CoreThread..."
  loop True
 where
  loop False = do
    logInfo "Terminating!"
    return ()
  loop True = do
    msg <- atomically $ readTBQueue queue
    case msg of
      Quit -> loop False
      _    -> do
        processMsg msg
        loop True


processMsg
  :: ( MonadUnliftIO m
     , MonadReader env m
     , HasRaiseEvent env
     , HasDataModel env
     , HasLogFunc env
     )
  => InterfaceAction
  -> m ()
processMsg Quit                           = return ()
processMsg (ImportMIB path serializePath) = importMIB path serializePath



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
      liftIO $ raiseEvent env (EVAlarms (EVMIBLoaded model))

