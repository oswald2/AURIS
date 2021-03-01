{-# LANGUAGE OverloadedStrings
    , BangPatterns
    , TemplateHaskell
    , NoImplicitPrelude
    , RecordWildCards
    , OverloadedLabels
#-}
module GUI.MainWindow
    ( MainWindow(..)
    , TMPacketTab(..)
    , createMainWindow
    , mwAddTMPacket
    , mwAddTMFrame
    , mwAddTMParameters
    , mwAddTMParameterDefinitions
    , mwAddVerifRqst
    , mwReleaseRqst
    , mwDisplayRqstVerification
    , mwSetMission
    , mwMessageDisplay
    , mwFrameTab
    , mwSetConnectionState
    , mwInitialiseDataModel
    , mwTimerLabelCB
    , mwWindow
    , mwMenuBar
    , mwProgress
    , mwConnTab
    , mwTCTab
    , mwTCHistory
    , mwMenuItemImportMIB
    , mwLiveState
    ) where

import           RIO
import qualified RIO.Text                      as T
--import qualified Data.Text.Encoding            as T
import qualified RIO.Vector                    as V
import           RIO.List                       ( sortBy )
import           Control.Lens                   ( makeLenses )

import qualified Data.HashTable.ST.Basic       as HT

import           GUI.TMPacketTab
import           GUI.TMFrameTab
import           GUI.TMParamTab
import           GUI.ConnectionTab
import           GUI.TCTab
import           GUI.TCHistory
import           GUI.Utils
import           GUI.Logo
import           GUI.MessageDisplay
import           GUI.MessageDetails
import           GUI.About


import           Data.PUS.TMPacket
import           Data.PUS.ExtractedDU
import           Data.PUS.TMFrame
import           Data.PUS.TCRequest             ( TCRequest )
import           Data.PUS.LiveState

import           Protocol.ProtocolInterfaces

import           Data.DataModel

import           Data.TM.Parameter
import           Data.TM.TMParameterDef

import           General.Time
import           General.PUSTypes               ( RequestID )

import           Verification.Verification      ( Verification )

import           GI.Gtk                        as Gtk
import           GI.GtkSource
import qualified GI.GtkSource.Objects.Buffer   as BUF
                                                ( bufferNew )

--import           Data.FileEmbed

import           AurisConfig



data MainWindow = MainWindow
    { _mwWindow            :: !Gtk.ApplicationWindow
    , _mwMenuBar           :: !Gtk.MenuBar
    , _mwProgress          :: !Gtk.ProgressBar
    , _mwMessageDisplay    :: !MessageDisplay
    , _mwTMPTab            :: !TMPacketTab
    , _mwTMParamTab        :: !TMParamTab
    , _mwMission           :: !Gtk.Label
    , _mwFrameTab          :: !TMFrameTab
    , _mwConnTab           :: !ConnectionTab
    , _mwTCTab             :: !TCTab
    , _mwTCHistory         :: !TCHistory
    , _mwTimeLabel         :: !Label
    , _mwMenuItemImportMIB :: !Gtk.MenuItem
    , _mwLiveState         :: TVar LiveState
    }
makeLenses ''MainWindow


mwAddTMPacket :: MainWindow -> TMPacket -> IO ()
mwAddTMPacket window pkt = do
    tmpTabAddRow (window ^. mwTMPTab) pkt

mwAddTMFrame :: MainWindow -> ExtractedDU TMFrame -> IO ()
mwAddTMFrame window = tmfTabAddRow (window ^. mwFrameTab)

mwAddTMParameters :: MainWindow -> Vector TMParameter -> IO ()
mwAddTMParameters window params = do
    addParameterValues (window ^. mwTMParamTab) params

mwAddTMParameterDefinitions :: MainWindow -> Vector TMParameterDef -> IO ()
mwAddTMParameterDefinitions window paramDefs = do
    addParameterDefinitions (window ^. mwTMParamTab) paramDefs


mwAddVerifRqst :: MainWindow -> TCRequest -> Verification -> IO ()
mwAddVerifRqst window rqst verif = do
    tcHistAddNewRqst (window ^. mwTCHistory) rqst verif

mwDisplayRqstVerification :: MainWindow -> RequestID -> Verification -> IO ()
mwDisplayRqstVerification window rqstID verif = do
    tcHistDisplayRqstVerification (window ^. mwTCHistory) rqstID verif

mwReleaseRqst :: MainWindow -> RequestID -> SunTime -> Verification -> IO ()
mwReleaseRqst window rqstID releaseTime verif = do
    tcHistReleaseRqst (window ^. mwTCHistory) rqstID releaseTime verif

mwSetMission :: MainWindow -> Text -> IO ()
mwSetMission window = labelSetLabel (window ^. mwMission)


mwInitialiseDataModel :: MainWindow -> DataModel -> IO ()
mwInitialiseDataModel window model = do
    let paramDefs =
            V.fromList . sortBy s . map snd . HT.toList $ model ^. dmParameters
        s p1 p2 = compare (p1 ^. fpName) (p2 ^. fpName)
    mwAddTMParameterDefinitions window paramDefs

    -- also add the displas 
    addGrdDefinitions (window ^. mwTMParamTab) (model ^. dmGRDs)


-- gladeFile :: Text
-- gladeFile =
--     T.decodeUtf8 $(makeRelativeToProject "src/MainWindow.glade" >>= embedFile)




createMainWindow :: AurisConfig -> IO MainWindow
createMainWindow cfg = do
    -- builder <- builderNewFromString gladeFile
    --                                 (fromIntegral (T.length gladeFile))
    builder           <- builderNewFromResource "/auris/data/MainWindow.glade"

    window            <- getObject builder "mainWindow" ApplicationWindow
    mainMenuBar       <- getObject builder "mainMenuBar" MenuBar
    missionLabel      <- getObject builder "labelMission" Label
    progressBar       <- getObject builder "progressBar" ProgressBar
    aboutItem         <- getObject builder "menuitemAbout" MenuItem
    logo              <- getObject builder "logo" Image
    timeLabel         <- getObject builder "labelTime" Label
    configTextView    <- getObject builder "sourceViewConfig" View

    menuItemQuit      <- getObject builder "menuItemQuit" MenuItem
    menuItemImportMIB <- getObject builder "menuItemImportMIB" MenuItem

    menuItemLoadTC    <- getObject builder "menuItemLoadTCFile" MenuItem
    menuItemSaveTC    <- getObject builder "menuItemSaveTCFile" MenuItem
    menuItemSaveTCAs  <- getObject builder "menuItemSaveTCFileAs" MenuItem

    btStyle           <- getObject builder "btCfgStyle" StyleSchemeChooserButton
    btApply           <- getObject builder "btCfgApply" Button

    -- create the message display
    msgDetails        <- createMsgDetailWindow window builder
    msgDisp           <- createMessageDisplay msgDetails builder

    -- create the tabs in the notebook
    tmfTab            <- createTMFTab builder
    tmpTab            <- createTMPTab window builder
    paramTab          <- createTMParamTab builder
    connTab           <- createConnectionTab (aurisPusConfig cfg) builder
    tcTab             <- createTCTab window builder
    tcHistory         <- createTCHistory window builder


    setLogo logo 65 65

    liveState <- newTVarIO defaultLiveState

    let gui = MainWindow { _mwWindow            = window
                         , _mwMenuBar           = mainMenuBar
                         , _mwMission           = missionLabel
                         , _mwProgress          = progressBar
                         , _mwMessageDisplay    = msgDisp
                         , _mwFrameTab          = tmfTab
                         , _mwTMPTab            = tmpTab
                         , _mwTimeLabel         = timeLabel
                         , _mwTMParamTab        = paramTab
                         , _mwConnTab           = connTab
                         , _mwTCTab             = tcTab
                         , _mwTCHistory         = tcHistory
                         , _mwMenuItemImportMIB = menuItemImportMIB
                         , _mwLiveState         = liveState
                         }

    void $ Gtk.on aboutItem #activate $ do
        diag <- createAboutDialog
        void $ dialogRun diag
        widgetHide diag

    void $ Gtk.on menuItemQuit #activate $ do
        widgetDestroy window
        mainQuit

    void $ Gtk.on menuItemLoadTC #activate $ tcTabLoadFile tcTab
    void $ Gtk.on menuItemSaveTC #activate $ tcTabSaveFile tcTab
    void $ Gtk.on menuItemSaveTCAs #activate $ tcTabSaveFileAs tcTab

    lm               <- languageManagerNew
    styleViewMgr     <- styleSchemeManagerGetDefault

    -- schemeIds <- styleSchemeManagerGetSchemeIds styleViewMgr
    -- T.putStrLn $ "Schemes: " <> T.pack (show schemeIds)

    scheme           <- styleSchemeManagerGetScheme styleViewMgr "classic"
    configTextBuffer <- BUF.bufferNew (Nothing :: Maybe TextTagTable)
    bufferSetStyleScheme configTextBuffer (Just scheme)

    let t = configPretty cfg

    lang <- languageManagerGetLanguage lm "json"
    bufferSetLanguage configTextBuffer lang

    textBufferSetText configTextBuffer t (fromIntegral (T.length t))
    textViewSetBuffer configTextView (Just configTextBuffer)

    void $ Gtk.on btApply #clicked $ do
        s <- styleSchemeChooserGetStyleScheme btStyle
        bufferSetStyleScheme configTextBuffer (Just s)

    return gui




mwTimerLabelCB :: MainWindow -> IO Bool
mwTimerLabelCB window = do
    now <- getCurrentTime
    labelSetLabel (window ^. mwTimeLabel) (displayTimeMilli now)
    return True



mwSetConnectionState
    :: MainWindow -> ProtocolInterface -> ConnType -> ConnectionState -> IO ()
mwSetConnectionState g = connTabSetConnection (_mwConnTab g)


