{-# LANGUAGE OverloadedStrings
    , BangPatterns
    , TemplateHaskell
    , NoImplicitPrelude
    , RecordWildCards
#-}
module GUI.MainWindow
  ( MainWindowFluid(..)
  , MainWindow(..)
  , TMPacketTabFluid(..)
  , TMPacketTab(..)
  , MainMenu(..)
  , NctrsConnGroup(..)
  , CncConnGroup(..)
  , EdenConnGroup(..)
  , createMainWindow
  , scrollNew
  , mwWindow
  , mmOpenFile
  , mmSaveFile
  , mmAbout
  , mwProgress
  , mwTabs
  , mwTMPTab
  , mwTMPGroup
  , mwTMPHeaderGroup
  , mwTMFGroup
  , mwMessageDisplay
  , mwAddTMPacket
  , mwAddTMFrame
  , mwSetTMParameters
  , mwAddTMParameters
  , mwAddTMParameterDefinitions
  , mwSetMission
  , mwDeskHeaderGroup
  , mwLogoBox
  , mwMainMenu
  , mwAboutWindow
  , mwFrameTab
  , mwNCTRSConnection
  , mwInitialiseDataModel
  )
where

import           RIO
--import qualified RIO.Text                      as T
-- import qualified Data.Text.IO                  as T
import qualified RIO.Vector                    as V
import           RIO.List                       ( sortBy )
import           Control.Lens                   ( makeLenses )

import qualified Data.HashTable.ST.Basic       as HT

import           Graphics.UI.FLTK.LowLevel.FLTKHS
import qualified Graphics.UI.FLTK.LowLevel.FL  as FL

import           GUI.TMPacketTab
import           GUI.TMFrameTab
import           GUI.TMParamTab
import           GUI.Colors
--import           GUI.Utils
import           GUI.Logo
import           GUI.About

import           Data.PUS.TMPacket
import           Data.PUS.ExtractedDU
import           Data.PUS.TMFrame

import           Data.DataModel

import           Data.TM.Parameter
import           Data.TM.TMParameterDef

import           General.Time



data MainMenu = MainMenu {
    _mmOpenFile :: Ref MenuItemBase
  , _mmSaveFile :: Ref MenuItemBase
  , _mmFullScreen :: Ref MenuItemBase
  , _mmFullScreenOff :: Ref MenuItemBase
  , _mmAbout :: Ref MenuItemBase
  }
makeLenses ''MainMenu


data NctrsConnGroup = NctrsConnGroup {
  _mfNctrsConnGroup :: Ref Group
  , _mfNctrsTCConn :: Ref Box
  , _mfNctrsTMConn :: Ref Box
  , _mfNctrsADMConn :: Ref Box
  , _mfNctrsTClabel :: Ref Box
  , _mfNctrsTMlabel :: Ref Box
  , _mfNctrsADMlabel :: Ref Box
  }
makeLenses ''NctrsConnGroup

txtConnected :: Text
txtConnected = "CONNECTED"

txtDisconnected :: Text
txtDisconnected = "DISCONNECTED"

initNctrsConnGroup :: NctrsConnGroup -> IO ()
initNctrsConnGroup NctrsConnGroup {..} = do
  mcsGroupingSetColor _mfNctrsConnGroup
  mcsBoxLabel _mfNctrsTClabel
  mcsBoxLabel _mfNctrsTMlabel
  mcsBoxLabel _mfNctrsADMlabel

  mcsBoxAlarm _mfNctrsTCConn  txtDisconnected
  mcsBoxAlarm _mfNctrsTMConn  txtDisconnected
  mcsBoxAlarm _mfNctrsADMConn txtDisconnected

data CncConnGroup = CncConnGroup {
  _mfCncConnGroup :: Ref Group
  , _mfCncTCConn :: Ref Box
  , _mfCncTMConn :: Ref Box
  , _mfCncLabelTCConn :: Ref Box
  , _mfCncLabelTMConn :: Ref Box
  }
makeLenses ''CncConnGroup

initCncConnGroup :: CncConnGroup -> IO ()
initCncConnGroup CncConnGroup {..} = do
  mcsGroupingSetColor _mfCncConnGroup
  mcsBoxLabel _mfCncLabelTCConn
  mcsBoxLabel _mfCncLabelTMConn

  mcsBoxAlarm _mfCncTCConn txtDisconnected
  mcsBoxAlarm _mfCncTMConn txtDisconnected


data EdenConnGroup = EdenConnGroup {
  _mwEdenConnGroup :: Ref Group
  , _mwEdenConnBox :: Ref Box
  , _mwEdenLabelConn :: Ref Box
  }
makeLenses ''EdenConnGroup

initEdenConnGroup :: EdenConnGroup -> IO ()
initEdenConnGroup EdenConnGroup {..} = do
  mcsGroupingSetColor _mwEdenConnGroup
  mcsBoxLabel _mwEdenLabelConn
  mcsBoxAlarm _mwEdenConnBox txtDisconnected




data MainWindowFluid = MainWindowFluid {
    _mfWindow :: Ref Window
    , _mfProgress :: Ref Progress
    , _mfTabs :: Ref Tabs
    , _mfTMPTab :: TMPacketTabFluid
    , _mfTMParamTab :: TMParamTabFluid
    , _mfTMPGroup :: Ref Group
    , _mfTMFGroup :: Ref Group
    , _mfTMPHeaderGroup :: Ref Group
    , _mfMessageDisplay :: Ref Browser
    , _mfMission :: Ref Output
    , _mfDeskHeaderGroup :: Ref Group
    , _mfLogoGroup :: Ref Group
    , _mfLogoBox :: Ref Box
    , _mfMainScrolled :: Ref Scrolled
    , _mfMainMenu :: MainMenu
    , _mfFrameTab :: TMFrameTabFluid
    , _mfNCTRSConn :: NctrsConnGroup
    , _mfCnCConn :: CncConnGroup
    , _mfEdenConn :: EdenConnGroup
    , _mfTimeLabel :: Ref Box
    }


data MainWindow = MainWindow {
    _mwWindow :: Ref Window
    , _mwProgress :: Ref Progress
    , _mwTabs :: Ref Tabs
    , _mwTMPTab :: TMPacketTab
    , _mwTMParamTab :: TMParamTab
    , _mwTMPGroup :: Ref Group
    , _mwTMPHeaderGroup :: Ref Group
    , _mwTMFGroup :: Ref Group
    , _mwMessageDisplay :: Ref Browser
    , _mwMission :: Ref Output
    , _mwDeskHeaderGroup :: Ref Group
    , _mwLogoBox :: Ref Box
    , _mwMainMenu :: MainMenu
    , _mwAboutWindow :: AboutWindowFluid
    , _mwFrameTab :: TMFrameTab
    , _mwNCTRSConn :: NctrsConnGroup
    , _mwCnCConn :: CncConnGroup
    , _mwEdenConn :: EdenConnGroup
    , _mwTimeLabel :: Ref Box
    }
makeLenses ''MainWindow


scrollNew :: Rectangle -> Maybe Text -> IO (Ref Scrolled)
scrollNew = scrolledNew

mwAddTMPacket :: MainWindow -> TMPacket -> IO ()
mwAddTMPacket window pkt = do
  tmpTabAddRow (window ^. mwTMPTab) pkt

mwSetTMParameters :: MainWindow -> TMPacket -> IO ()
mwSetTMParameters window pkt = do
  tmpTabDetailSetValues (window ^. mwTMPTab) pkt

mwAddTMFrame :: MainWindow -> ExtractedDU TMFrame -> IO ()
mwAddTMFrame window frame = do
  tmfTabAddRow (window ^. mwFrameTab) frame

mwAddTMParameters :: MainWindow -> Vector TMParameter -> IO ()
mwAddTMParameters window params = do
  addParameterValues (window ^. mwTMParamTab) params

mwAddTMParameterDefinitions :: MainWindow -> Vector TMParameterDef -> IO ()
mwAddTMParameterDefinitions window paramDefs = do
  addParameterDefinitions (window ^. mwTMParamTab) paramDefs


mwSetMission :: MainWindow -> Text -> IO ()
mwSetMission window mission = do
  void $ setValue (window ^. mwMission) mission


mwInitialiseDataModel :: MainWindow -> DataModel -> IO ()
mwInitialiseDataModel window model = do
  let paramDefs =
        V.fromList . sortBy s . map snd . HT.toList $ model ^. dmParameters
      s p1 p2 = compare (p1 ^. fpName) (p2 ^. fpName)
  mwAddTMParameterDefinitions window paramDefs
  return ()



createMainWindow :: MainWindowFluid -> AboutWindowFluid -> IO MainWindow
createMainWindow MainWindowFluid {..} aboutWindow = do
  tmpTab   <- createTMPTab _mfTMPTab
  tmfTab   <- createTMFTab _mfFrameTab
  paramTab <- createTMParamTab _mfTMParamTab
  mcsWindowSetColor _mfWindow

  -- maximizeWindow _mfWindow

  mcsScrolledSetColor _mfMainScrolled
  mcsTabsSetColor _mfTabs

  setResizable _mfTabs (Just _mfTMPGroup)

  mcsGroupSetColor _mfTMPGroup
  mcsGroupSetColor _mfTMFGroup
  mcsGroupSetColor _mfTMPHeaderGroup
  mcsHeaderGroupSetColor _mfDeskHeaderGroup
  mcsProgressSetColor _mfProgress

  mcsBrowserSetColor _mfMessageDisplay

  mcsOutputSetColor _mfMission

  initLogo _mfLogoBox aurisLogo

  initNctrsConnGroup _mfNCTRSConn
  initCncConnGroup _mfCnCConn
  initEdenConnGroup _mfEdenConn

  mcsBoxTime _mfTimeLabel

  -- mcsWidgetSetColor _mfOpenFile
  -- mcsWidgetSetColor _mfSaveFile
--   mcsButtonSetColor _mfArmButton
--   mcsButtonSetColor _mfGoButton
  let mainWindow = MainWindow { _mwWindow          = _mfWindow
                              , _mwProgress        = _mfProgress
                              , _mwTabs            = _mfTabs
                              , _mwTMPTab          = tmpTab
                              , _mwTMParamTab      = paramTab
                              , _mwTMPGroup        = _mfTMPGroup
                              , _mwTMFGroup        = _mfTMFGroup
                              , _mwTMPHeaderGroup  = _mfTMPHeaderGroup
                              , _mwMessageDisplay  = _mfMessageDisplay
                              , _mwMission         = _mfMission
                              , _mwDeskHeaderGroup = _mfDeskHeaderGroup
                              , _mwLogoBox         = _mfLogoBox
                              , _mwMainMenu        = _mfMainMenu
                              , _mwAboutWindow     = aboutWindow
                              , _mwFrameTab        = tmfTab
                              , _mwNCTRSConn       = _mfNCTRSConn
                              , _mwCnCConn         = _mfCnCConn
                              , _mwEdenConn        = _mfEdenConn
                              , _mwTimeLabel       = _mfTimeLabel
                              }

  initTimer mainWindow

  --rect <- getRectangle _mfWindow
  setCallback (mainWindow ^. mwMainMenu . mmFullScreen) (fullScreen mainWindow)
  setCallback (mainWindow ^. mwMainMenu . mmFullScreenOff)
              (fullScreenOff mainWindow)

  pure mainWindow


fullScreen :: MainWindow -> Ref MenuItemBase -> IO ()
fullScreen window _ = makeFullscreen (window ^. mwWindow)

fullScreenOff :: MainWindow -> Ref MenuItemBase -> IO ()
fullScreenOff window _ = fullscreenOff (window ^. mwWindow) Nothing



initTimer :: MainWindow -> IO ()
initTimer window = do
  void $ FL.addTimeout 1 (timerCB window)


timerCB :: MainWindow -> IO ()
timerCB window = do
  now <- getCurrentTime
  setLabel (window ^. mwTimeLabel) (displayTimeMilli now)
  void $ FL.repeatTimeout 1 (timerCB window)




mwNCTRSConnection :: MainWindow -> Bool -> IO ()
mwNCTRSConnection MainWindow {..} True =
  mcsBoxGreen (_mwNCTRSConn ^. mfNctrsTMConn) txtConnected
mwNCTRSConnection MainWindow {..} False =
  mcsBoxAlarm (_mwNCTRSConn ^. mfNctrsTMConn) txtConnected


