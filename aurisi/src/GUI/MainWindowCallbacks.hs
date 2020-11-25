{-# LANGUAGE OverloadedStrings
    , BangPatterns
    , GeneralizedNewtypeDeriving
    , DeriveGeneric
    , RecordWildCards
    , NoImplicitPrelude
#-}
module GUI.MainWindowCallbacks
  ( GUI.MainWindowCallbacks.setupCallbacks
  , importMIB
  )
where


import           RIO
import           RIO.Partial                    ( toEnum )

import qualified GI.Gtk                        as Gtk

import           GUI.MainWindow
import           GUI.TMFrameTab
import           GUI.TCTab 

import           Interface.Interface

import           System.Directory
import           System.FilePath

import           AurisConfig


setupCallbacks :: MainWindow -> Interface -> IO ()
setupCallbacks window interface = do
  GUI.TMFrameTab.setupCallbacks (window ^. mwFrameTab)
  GUI.TCTab.setupCallbacks (window ^. mwTCTab) interface
  void $ Gtk.on (window ^. mwMenuItemImportMIB) #activate $ importMIB window interface 



importMIB :: MainWindow -> Interface -> IO ()
importMIB gui interface = do
  fc <- Gtk.fileChooserNativeNew (Just "Import MIB, select directory...")
                                 (Just (gui ^. mwWindow))
                                 Gtk.FileChooserActionSelectFolder
                                 Nothing
                                 Nothing

  res <- Gtk.nativeDialogRun fc
  case toEnum (fromIntegral res) of
    Gtk.ResponseTypeAccept -> do
      fileName <- Gtk.fileChooserGetFilename fc
      forM_ fileName $ \fn -> do
        home <- liftIO getHomeDirectory
        let serializedPath = home </> configPath </> defaultMIBFile
        callInterface interface actionImportMIB fn serializedPath
    _ -> return ()


  -- chooser <- nativeFileChooserNew (Just BrowseDirectory)
  -- setTitle chooser "Import MIB, select directory..."
  -- result <- showWidget chooser
  -- case result of
  --   NativeFileChooserPicked -> do
  --     dir' <- getFilename chooser
  --     case dir' of
  --       Nothing  -> return ()
  --       Just dir -> do
  --         home <- liftIO getHomeDirectory
  --         let serializedPath = home </> configPath </> defaultMIBFile
  --         callInterface interface actionImportMIB (T.unpack dir) serializedPath
  --   NativeFileChooserError -> do
  --     msg <- getErrmsg chooser
  --     forM_ msg flAlert
  --   _ -> return ()
