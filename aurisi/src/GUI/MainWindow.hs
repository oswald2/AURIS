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
  , mwSetMission
  , mwMessageDisplay
  , mwFrameTab
  , mwSetConnectionState
  , mwInitialiseDataModel
  , mwTimerLabelCB
  , mwWindow
  , mwProgress
  , mwConnTab
  , mwTCTab 
  , mwMenuItemImportMIB
  )
where

import           RIO
import qualified RIO.Text                      as T
import qualified Data.Text.Encoding            as T
import qualified RIO.Vector                    as V
import           RIO.List                       ( sortBy )
import           Control.Lens                   ( makeLenses )

import qualified Data.HashTable.ST.Basic       as HT

import           GUI.TMPacketTab
import           GUI.TMFrameTab
import           GUI.TMParamTab
import           GUI.ConnectionTab
import           GUI.TCTab
import           GUI.Utils
import           GUI.Logo
import           GUI.MessageDisplay
import           GUI.About

import           Data.PUS.TMPacket
import           Data.PUS.ExtractedDU
import           Data.PUS.TMFrame

import           Protocol.ProtocolInterfaces

import           Data.DataModel

import           Data.TM.Parameter
import           Data.TM.TMParameterDef

import           General.Time

import           GI.Gtk                        as Gtk
import           Data.FileEmbed

import           AurisConfig



data MainWindow = MainWindow {
    _mwWindow :: !Gtk.Window
    , _mwProgress :: !Gtk.ProgressBar
    , _mwMessageDisplay :: !MessageDisplay
    , _mwTMPTab :: !TMPacketTab
    , _mwTMParamTab :: !TMParamTab
    , _mwMission :: !Gtk.Label
    , _mwFrameTab :: !TMFrameTab
    , _mwConnTab :: !ConnectionTab
    , _mwTCTab :: !TCTab
    , _mwTimeLabel :: !Label
    , _mwMenuItemImportMIB :: !Gtk.MenuItem
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


gladeFile :: Text
gladeFile =
  T.decodeUtf8 $(makeRelativeToProject "src/MainWindow.glade" >>= embedFile)



createMainWindow :: AurisConfig -> IO MainWindow
createMainWindow cfg = do
  builder <- builderNewFromString gladeFile (fromIntegral (T.length gladeFile))

  window            <- getObject builder "mainWindow" Window
  missionLabel      <- getObject builder "labelMission" Label
  progressBar       <- getObject builder "progressBar" ProgressBar
  aboutItem         <- getObject builder "menuitemAbout" MenuItem
  logo              <- getObject builder "logo" Image
  timeLabel         <- getObject builder "labelTime" Label

  menuItemQuit      <- getObject builder "menuItemQuit" MenuItem
  menuItemImportMIB <- getObject builder "menuItemImportMIB" MenuItem

  -- create the tabs in the notebook
  tmfTab            <- createTMFTab builder
  tmpTab            <- createTMPTab builder
  paramTab          <- createTMParamTab builder
  connTab           <- createConnectionTab (aurisPusConfig cfg) builder
  tcTab             <- createTCTab window builder 

  -- create the message display
  msgDisp           <- createMessageDisplay builder

  setLogo logo 65 65

  let gui = MainWindow { _mwWindow            = window
                       , _mwMission           = missionLabel
                       , _mwProgress          = progressBar
                       , _mwMessageDisplay    = msgDisp
                       , _mwFrameTab          = tmfTab
                       , _mwTMPTab            = tmpTab
                       , _mwTimeLabel         = timeLabel
                       , _mwTMParamTab        = paramTab
                       , _mwConnTab           = connTab
                       , _mwTCTab             = tcTab 
                       , _mwMenuItemImportMIB = menuItemImportMIB
                       }

  void $ Gtk.on aboutItem #activate $ do
    diag <- createAboutDialog
    void $ dialogRun diag
    widgetHide diag

  void $ Gtk.on menuItemQuit #activate $ do
    widgetDestroy window
    mainQuit

  return gui



mwTimerLabelCB :: MainWindow -> IO Bool
mwTimerLabelCB window = do
  now <- getCurrentTime
  labelSetLabel (window ^. mwTimeLabel) (displayTimeMilli now)
  return True



mwSetConnectionState
  :: MainWindow -> ProtocolInterface -> ConnType -> ConnectionState -> IO ()
mwSetConnectionState g = connTabSetConnection (_mwConnTab g)


