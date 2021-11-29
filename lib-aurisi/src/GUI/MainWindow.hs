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
    , mwSetTMFrames
    , mwAddTMParameters
    , mwAddTMParameterDefinitions
    , mwAddVerifRqst
    , mwReleaseRqst
    , mwDisplayRqstVerification
    , mwSetMission
    , mwAddTMStatistic
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
    , mwSLETab
    , mwTCHistory
    , mwStatisticsTab
    , mwMenuItemImportMIB
    , mwMenuItemQuit
    , mwLiveState
    , mwSetSleRafState
    ) where

import           Control.Lens                   ( makeLenses )
import           RIO
import           RIO.List                       ( sortBy )
import qualified RIO.Text                      as T
--import qualified Data.Text.Encoding            as T
import qualified RIO.Vector                    as V

import qualified Data.HashTable.ST.Basic       as HT

import           GUI.About
import           GUI.ConnectionTab
import           GUI.DataModelTab
import           GUI.Logo
import           GUI.MessageDetails
import           GUI.MessageDisplay
import           GUI.SLEConnections             ( SleServiceStatus(..) )
import           GUI.SLETab
import           GUI.StatisticsTab
import           GUI.TCHistory
import           GUI.TCTab
import           GUI.TMFrameTab
import           GUI.TMPacketTab
import           GUI.TMParamTab
import           GUI.Utils


import           Data.PUS.ExtractedDU           ( ExtractedDU )
import           Data.PUS.LiveState             ( LiveState
                                                , defaultLiveState
                                                )
import           Data.PUS.TCRequest             ( TCRequest )
import           Data.PUS.TMFrame               ( TMFrame )
import           Data.PUS.TMPacket              ( TMPacket )

import           Protocol.ProtocolInterfaces    ( ConnType
                                                , ConnectionState
                                                , ProtocolInterface
                                                )

import           Data.DataModel                 ( DataModel
                                                , dmGRDs
                                                , dmParameters
                                                , dmTCs
                                                )

import           Data.TC.TCDef                  ( tcDefName )
import           Data.TM.Parameter              ( TMParameter )
import           Data.TM.TMParameterDef         ( TMParameterDef
                                                , fpName
                                                )
import           General.PUSTypes               ( RequestID )
import           General.Time                   ( SunTime
                                                , displayTimeMilli
                                                , getCurrentTime
                                                )

import           Data.PUS.Verification          ( Verification )

import           GI.Gtk                        as Gtk
import           GI.GtkSource
import qualified GI.GtkSource.Objects.Buffer   as BUF
                                                ( bufferNew )

--import           Data.FileEmbed

import           AurisConfig
import           Data.PUS.Events                ( TMStatistics )



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
    , _mwSLETab            :: !(Maybe SLETab)
    , _mwTCTab             :: !TCTab
    , _mwTCHistory         :: !TCHistory
    , _mwDataModelTab      :: !DataModelTab
    , _mwStatisticsTab     :: !StatisticsTab
    , _mwTimeLabel         :: !Label
    , _mwMenuItemImportMIB :: !Gtk.MenuItem
    , _mwMenuItemQuit      :: !Gtk.MenuItem
    , _mwLiveState         :: TVar LiveState
    }
makeLenses ''MainWindow


mwAddTMPacket :: MainWindow -> ExtractedDU TMPacket -> IO ()
mwAddTMPacket window pkt = do
    tmpTabAddRow (window ^. mwTMPTab) pkt

mwAddTMFrame :: MainWindow -> ExtractedDU TMFrame -> IO ()
mwAddTMFrame window = tmfTabAddRow (window ^. mwFrameTab)

mwSetTMFrames :: MainWindow -> [ExtractedDU TMFrame] -> IO ()
mwSetTMFrames window = tmfTabSetFrames (window ^. mwFrameTab)

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

    -- set the data model viewer
    dataModelTabSetModel (window ^. mwDataModelTab) model

    -- set the TCs in the TC browser 
    let tcs = sortBy st . map snd . HT.toList $ model ^. dmTCs
        st tc1 tc2 = compare (tc1 ^. tcDefName) (tc2 ^. tcDefName)
    tcTabSetTCs (window ^. mwTCTab) tcs

-- gladeFile :: Text
-- gladeFile =
--     T.decodeUtf8 $(makeRelativeToProject "src/MainWindow.glade" >>= embedFile)

mwAddTMStatistic :: MainWindow -> TMStatistics -> IO ()
mwAddTMStatistic window stats =
    statisticsTabDisplayStats (window ^. mwStatisticsTab) stats

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

    btApply           <- getObject builder "buttonConfigApplyStyle" Button
    btStyle           <- getObject builder
                                   "buttonConfigSelectStyle"
                                   StyleSchemeChooserButton

    -- create the message display
    msgDetails    <- createMsgDetailWindow window builder
    msgDisp       <- createMessageDisplay msgDetails builder

    -- create the tabs in the notebook
    tmfTab        <- createTMFTab window builder
    tmpTab        <- createTMPTab window builder
    paramTab      <- createTMParamTab builder
    connTab       <- createConnectionTab (aurisPusConfig cfg) builder
    sleTab        <- createSLETab (aurisPusConfig cfg) builder
    tcTab         <- createTCTab (aurisPusConfig cfg) window builder
    tcHistory     <- createTCHistory window builder
    dataModelTab  <- createDataModelTab window builder
    statisticsTab <- createStatisticsTab builder

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
                         , _mwSLETab            = sleTab
                         , _mwTCTab             = tcTab
                         , _mwTCHistory         = tcHistory
                         , _mwDataModelTab      = dataModelTab
                         , _mwStatisticsTab     = statisticsTab
                         , _mwMenuItemImportMIB = menuItemImportMIB
                         , _mwMenuItemQuit      = menuItemQuit
                         , _mwLiveState         = liveState
                         }

    void $ Gtk.on aboutItem #activate $ do
        diag <- createAboutDialog
        void $ dialogRun diag
        widgetHide diag

    void $ Gtk.on menuItemLoadTC #activate $ tcTabLoadFile tcTab
    void $ Gtk.on menuItemSaveTC #activate $ tcTabSaveFile tcTab
    void $ Gtk.on menuItemSaveTCAs #activate $ tcTabSaveFileAs tcTab

    lm               <- languageManagerNew
    styleViewMgr     <- styleSchemeManagerGetDefault

    -- schemeIds <- styleSchemeManagerGetSchemeIds styleViewMgr
    -- T.putStrLn $ "Schemes: " <> T.pack (show schemeIds)

    scheme           <- styleSchemeManagerGetScheme styleViewMgr "kate"
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


mwSetSleRafState :: MainWindow -> Text -> SleServiceStatus -> IO ()
mwSetSleRafState gui sii status = do
    forM_ (gui ^. mwSLETab) $ \tab -> updateRAFStatus tab sii status
