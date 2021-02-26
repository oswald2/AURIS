module Application.DataModel
    ( loadDataModel
    , loadDataModelDef
    , LoadFrom(..)
    ) where

import           RIO

import qualified RIO.Text                      as T

import           Data.DataModel
import           Data.MIB.LoadMIB

import           System.Directory
import           System.FilePath

import           Text.Builder

import           GHC.Compact

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
-- representation. If the model could not be loaded, returns an empty model.
loadDataModelDef
    :: (MonadUnliftIO m, MonadReader env m, HasLogFunc env)
    => LoadFrom
    -> m (Compact DataModel)
loadDataModelDef (LoadFromMIB str serializedPath) = do
    res <- loadMIB str
    case res of
        Left err -> do
            logError
                $  display ("Error on importing MIB: " :: Text)
                <> display err
            liftIO $ compact Data.DataModel.empty
        Right model -> do
            logInfo $ display ("Successfully imported MIB." :: Text)
            liftIO $ createDirectoryIfMissing True
                                              (takeDirectory serializedPath)
            logInfo "Writing data model to disk..."
            writeDataModel serializedPath (getCompact model)
            logInfo "Data Model written."
            return model
loadDataModelDef (LoadFromSerialized path) = do
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
                    liftIO $ compact Data.DataModel.empty
                Right model -> do
                    logInfo $ display ("Successfully loaded data model" :: Text)
                    liftIO $ compact model
        else do
            logInfo
                $  display ("Data model file '" :: Text)
                <> display (T.pack path)
                <> display ("' does not exist." :: Text)
            liftIO $ compact Data.DataModel.empty





-- | Load a data model either from MIB or directly from the serialized 
-- representation.
loadDataModel
    :: (MonadUnliftIO m, MonadReader env m, HasLogFunc env)
    => LoadFrom
    -> m (Either Text (Compact DataModel))
loadDataModel (LoadFromMIB str serializedPath) = do
    res <- loadMIB str
    case res of
        Left  err   -> return (Left err)
        Right model -> do
            logDebug $ display ("Successfully imported MIB." :: Text)
            liftIO $ createDirectoryIfMissing True
                                              (takeDirectory serializedPath)
            logDebug "Writing data model to disk..."
            writeDataModel serializedPath (getCompact model)
            logDebug "Data Model written."
            return (Right model)
loadDataModel (LoadFromSerialized path) = do
    ex <- liftIO $ doesFileExist path
    if ex
        then do
            logDebug "calling readDataModel..."
            res <- readDataModel path
            case res of 
              Left err -> return $ Left err 
              Right model -> do
                cmodel <- liftIO $ try $ compact model 
                case cmodel of 
                  Left e -> return $ Left $ "Error on compacting model: " <> T.pack (show (e :: SomeException))
                  Right m -> return $ Right m                
        else do
            return
                $  Left
                $  run
                $  text "Data model file '"
                <> text (T.pack path)
                <> text "' does not exist."
