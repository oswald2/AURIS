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
  , mwSetMission
  , mwDeskHeaderGroup
  , mwLogoBox
  , mwMainMenu
  , mwAboutWindow
  , mwFrameTab
  )
where

import           RIO
import qualified RIO.Text                      as T
import qualified Data.Text.IO                  as T
import           Control.Lens                   ( makeLenses )

import           Graphics.UI.FLTK.LowLevel.FLTKHS

import           GUI.TMPacketTab
import           GUI.TMFrameTab
import           GUI.Colors
import           GUI.Utils
import           GUI.Logo
import           GUI.About

import           Data.PUS.TMPacket
import           Data.PUS.ExtractedDU
import           Data.PUS.TMFrame


data MainMenu = MainMenu {
    _mmOpenFile :: Ref MenuItemBase
  , _mmSaveFile :: Ref MenuItemBase
  , _mmAbout :: Ref MenuItemBase
  }
makeLenses ''MainMenu

data MainWindowFluid = MainWindowFluid {
    _mfWindow :: Ref Window
    , _mfProgress :: Ref Progress
    , _mfTabs :: Ref Tabs
    , _mfTMPTab :: TMPacketTabFluid
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
    }


data MainWindow = MainWindow {
    _mwWindow :: Ref Window
    , _mwProgress :: Ref Progress
    , _mwTabs :: Ref Tabs
    , _mwTMPTab :: TMPacketTab
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


mwSetMission :: MainWindow -> Text -> IO ()
mwSetMission window mission = do
  void $ setValue (window ^. mwMission) mission


createMainWindow :: MainWindowFluid -> AboutWindowFluid -> IO MainWindow
createMainWindow MainWindowFluid {..} aboutWindow = do
  tmpTab <- createTMPTab _mfTMPTab
  tmfTab <- createTMFTab _mfFrameTab
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

  -- mcsWidgetSetColor _mfOpenFile
  -- mcsWidgetSetColor _mfSaveFile
--   mcsButtonSetColor _mfArmButton
--   mcsButtonSetColor _mfGoButton
  let mainWindow = MainWindow { _mwWindow          = _mfWindow
                              , _mwProgress        = _mfProgress
                              , _mwTabs            = _mfTabs
                              , _mwTMPTab          = tmpTab
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
                              }
  pure mainWindow



