module Application.DataModel
  ( loadDataModel
  , LoadFrom(..)
  )
where

import           RIO

import qualified RIO.Text                      as T

import           Data.DataModel
import           Data.MIB.LoadMIB

import           System.Directory
import           System.FilePath


-- | Specifies from where to load a 'DataModel'. 
data LoadFrom =
  -- | First is to load from a MIB directory. It needs the path 
  -- to the MIB directory first and a path to the resulting serialized 
  -- file 
  LoadFromMIB FilePath FilePath
  -- | Loads the model directly from the serialized representation. 
  -- Needs a full path from to the file
  | LoadFromSerialized FilePath


-- | Load a data model either from MIB or directly from the serialized 
-- representation.
loadDataModel
  :: (MonadUnliftIO m, MonadReader env m, HasLogFunc env)
  => LoadFrom
  -> m DataModel
loadDataModel (LoadFromMIB str serializedPath) = do
  res <- loadMIB str
  case res of
    Left err -> do
      logError $ display ("Error on importing MIB: " :: Text) <> display err
      return Data.DataModel.empty
    Right model -> do
      logInfo $ display ("Successfully imported MIB." :: Text)
      liftIO $ createDirectoryIfMissing True (takeDirectory serializedPath)
      logInfo "Writing data model to disk..."
      writeDataModel serializedPath model
      logInfo "Data Model written."
      return model
loadDataModel (LoadFromSerialized path) = do
  ex <- liftIO $ doesFileExist path
  if ex
    then do
      logDebug "calling readDataModel..."
      res <- readDataModel path
      case res of
        Left err -> do
          logError
            $  display ("Error loading data model from " :: Text)
            <> display (T.pack path)
            <> display (": " :: Text)
            <> display err
          return Data.DataModel.empty
        Right model -> do
          logInfo $ display ("Successfully loaded data model" :: Text)
          return model
    else do
      logInfo
        $  display ("Data model file '" :: Text)
        <> display (T.pack path)
        <> display ("' does not exist." :: Text)
      return Data.DataModel.empty
