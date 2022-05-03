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
    ) where


import           RIO
import           RIO.Partial                    ( toEnum )

import qualified GI.Gtk                        as Gtk

import           GUI.MainWindow
import           GUI.SLETab                     ( setupCallbacks )
import           GUI.StatisticsTab
import           GUI.TCTab
import           GUI.TMFrameTab

-- import           Interface.Actions
import           Interface.Interface

import           System.Directory
import           System.FilePath

import           AurisConfig


setupCallbacks :: MainWindow -> AurisConfig -> Interface -> IO ()
setupCallbacks window cfg interface = do
    void $ Gtk.on (window ^. mwMenuItemQuit) #activate $ do
        callInterface interface actionQuit
        threadDelay 1_000_000
        Gtk.widgetDestroy (window ^. mwWindow)
        Gtk.mainQuit


    GUI.TMFrameTab.setupCallbacks (window ^. mwFrameTab) interface
    GUI.TCTab.setupCallbacks (window ^. mwTCTab) interface
    GUI.StatisticsTab.setupCallbacks (window ^. mwStatisticsTab) interface
    forM_ (window ^. mwSLETab)
        $ \sleTab -> GUI.SLETab.setupCallbacks sleTab interface

    void $ Gtk.on (window ^. mwMenuItemImportMIB) #activate $ importMIB
        window
        cfg
        interface





importMIB :: MainWindow -> AurisConfig -> Interface -> IO ()
importMIB gui cfg interface = do
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
                let serializedPath =
                        home
                            </> configPathInstance (aurisInstance cfg)
                            </> defaultMIBFile
                callInterface interface actionImportMIB fn serializedPath
        _ -> return ()

