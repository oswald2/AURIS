module AurisStart
    ( runApplication
    ) where

import           RIO

import           GUI.Definitions
import           GUI.MainWindow
import           GUI.MainWindowCallbacks
--import           GUI.About
import           GUI.Theme

import           AurisConfig
import           AurisInterface
import           AurisMissionSpecific
import           AurisProcessing

import qualified Data.GI.Gtk.Threading         as Gtk
import qualified GI.GLib.Constants             as GI
import qualified GI.GLib.Functions             as GI
import           GI.Gio                        as Gio
import qualified GI.Gtk                        as Gtk





runApplication :: AurisConfig -> Maybe FilePath -> IO () 
runApplication cfg mibPath = do
    Gtk.setCurrentThreadAsGUIThread
    app <- new
        Gtk.Application
        [ #applicationId := "auris.integrated"
        , #flags := [Gio.ApplicationFlagsFlagsNone]
        ]
    void $ Gtk.on app #activate $ appActivateHandler cfg mibPath app

    void $ Gio.applicationRun app Nothing



ui :: AurisConfig -> Gtk.Application -> IO MainWindow
ui cfg app = do
    window <- createMainWindow cfg
    Gtk.set (window ^. mwWindow) [#application := app]
    --Gtk.applicationSetMenubar app (Just (window ^.mwMenuBar))
    void $ Gtk.onWidgetDestroy (_mwWindow window) (Gio.applicationQuit app)
    Gtk.widgetShowAll (_mwWindow window)
    pure window



appActivateHandler :: AurisConfig -> Maybe FilePath -> Gtk.Application -> IO ()
appActivateHandler cfg mibPath app = do

        -- need to call it once in main before the GUI is started

        -- For some reason, on some systems the entries are displayed far too large
        -- (height too big, too much internal padding). This causes some displays not 
        -- to fit. So we globally set the min-width property for the entries.
    setEntryStyle

    -- create the main window
    mainWindow <- ui cfg app
    case aurisTheme cfg of
        ThemeDark  -> setDarkTheme
        ThemeLight -> setTheme

    iconTheme <- Gtk.iconThemeGetDefault
    Gtk.iconThemeAddResourcePath iconTheme "/auris/data"
    --names <- Gtk.iconThemeListIcons iconTheme Nothing
    --T.putStrLn $ "Icons: " <> T.pack (show names)
    --icon <- Gtk.iconThemeLoadIcon iconTheme "AurisLogo" 48 [IconLookupFlagsForceSvg]
    Gtk.windowSetIconName (mainWindow ^. mwWindow) (Just "AurisLogo")

    mwSetMission mainWindow (aurisMission cfg)

    -- setup the interface. We pass in a boolean if the system is configured to 
    -- use a database or not 
    (interface, _eventThread, coreQueue, queryQueue) <- initialiseInterface
        mainWindow
        (isJust (aurisDbConfig cfg))

    -- Setup the callbacks. Since we need the interface there, we can 
    -- do this only here
    setupCallbacks mainWindow interface

    -- determine the mission-specific functionality
    missionSpecific   <- getMissionSpecific cfg
    -- start the processing chains

    _processingThread <- async $ runProcessing cfg
                                               missionSpecific
                                               mibPath
                                               interface
                                               mainWindow
                                               coreQueue
                                               queryQueue

    void $ GI.timeoutAddSeconds GI.PRIORITY_DEFAULT
                                1
                                (mwTimerLabelCB mainWindow)
