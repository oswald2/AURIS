module Interface.CoreProcessor
  ( runCoreThread
  )
where


import           RIO

import           Application.DataModel
import           Interface.Interface


runCoreThread
  :: (MonadIO m, MonadReader env m, HasLogFunc env)
  => TBQueue InterfaceAction
  -> m ()
runCoreThread queue = loop True
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
  :: (MonadIO m, MonadReader env m, HasLogFunc env) => InterfaceAction -> m ()
processMsg Quit                           = return ()
processMsg (ImportMIB path serializePath) = importMIB path serializePath



importMIB
  :: (MonadIO m, MonadReader env m, HasLogFunc env)
  => FilePath
  -> FilePath
  -> m ()
importMIB path serializePath = do
  model' <- loadDataModel (LoadFromMIB path serializePath)
  case model' of
    Left err -> do
      ifRaiseEvent env (EVAlarms (EVMIBLoadError err))
    Right model -> do
      env <- ask
      setDataModel env model
      ifRaiseEvent env (EVAlarms (EVMIBLoaded model))

