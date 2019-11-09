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
    , createMainWindow
    , mwWindow
    , mwOpenFile
    , mwSaveFile
    , mwProgress
    , mwTabs
    , mwTMPTab
    , mwTMPGroup
    , mwTMPHeaderGroup
    , mwTMFGroup
    , mwMessageDisplay
    , mwAddTMPacket
    , mwSetTMParameters
    , mwSetMission
    , mwDeskHeaderGroup
    , mwTMParamDetailWindow
    )
where

import           RIO

import           Control.Lens                   ( makeLenses )

import           Graphics.UI.FLTK.LowLevel.FLTKHS

--import           Model.TMPacketModel
--import           Model.ScrollingTableModel

--import           GUI.TMPacketTable
import           GUI.TMPacketTab
--import           GUI.ScrollingTable
import           GUI.Colors
import           GUI.ParamDetailWindow
import           GUI.Utils

import           Data.PUS.TMPacket





data MainWindowFluid = MainWindowFluid {
    _mfWindow :: Ref Window
    , _mfOpenFile :: Ref MenuItemBase
    , _mfSaveFile :: Ref MenuItemBase
    , _mfProgress :: Ref Progress
    , _mfTabs :: Ref Tabs
    , _mfTMPTab :: TMPacketTabFluid
    , _mfTMPGroup :: Ref Group
    , _mfTMFGroup :: Ref Group
    , _mfTMPHeaderGroup :: Ref Group
    , _mfMessageDisplay :: Ref Browser
    , _mfMission :: Ref Output
    , _mfDeskHeaderGroup :: Ref Group
    }


data MainWindow = MainWindow {
    _mwWindow :: Ref Window
    , _mwOpenFile :: Ref MenuItemBase
    , _mwSaveFile :: Ref MenuItemBase
    , _mwProgress :: Ref Progress
    , _mwTabs :: Ref Tabs
    , _mwTMPTab :: TMPacketTab
    , _mwTMPGroup :: Ref Group
    , _mwTMPHeaderGroup :: Ref Group
    , _mwTMFGroup :: Ref Group
    , _mwTMParamDetailWindow :: ParamDetailWindow
    , _mwMessageDisplay :: Ref Browser
    , _mwMission :: Ref Output
    , _mwDeskHeaderGroup :: Ref Group
    }
makeLenses ''MainWindow


mwAddTMPacket :: MainWindow -> TMPacket -> IO ()
mwAddTMPacket window pkt = do
    tmpTabAddRow (window ^. mwTMPTab) pkt

mwSetTMParameters :: MainWindow -> TMPacket -> IO ()
mwSetTMParameters window pkt = do
  tmpTabDetailSetValues (window ^. mwTMPTab) pkt

mwSetMission :: MainWindow -> Text -> IO ()
mwSetMission window mission = do
  void $ setValue (window ^. mwMission) mission


createMainWindow :: MainWindowFluid -> ParamDetailWindowFluid -> IO MainWindow
createMainWindow MainWindowFluid {..} paramDetailWindow = do
    tmpTab <- createTMPTab _mfTMPTab
    mcsWindowSetColor _mfWindow

    maximizeWindow _mfWindow

    mcsTabsSetColor _mfTabs

    setResizable _mfTabs (Just _mfTMPGroup)

    mcsGroupSetColor _mfTMPGroup
    mcsGroupSetColor _mfTMFGroup
    mcsGroupSetColor _mfTMPHeaderGroup
    mcsHeaderGroupSetColor _mfDeskHeaderGroup

    mcsBrowserSetColor _mfMessageDisplay

    mcsOutputSetColor _mfMission

    pdetw <- createTMParamDetailWindow paramDetailWindow

    -- mcsWidgetSetColor _mfOpenFile
    -- mcsWidgetSetColor _mfSaveFile
  --   mcsButtonSetColor _mfArmButton
  --   mcsButtonSetColor _mfGoButton
    let mainWindow = MainWindow { _mwWindow              = _mfWindow
                                , _mwOpenFile            = _mfOpenFile
                                , _mwSaveFile            = _mfSaveFile
                                , _mwProgress            = _mfProgress
                                , _mwTabs                = _mfTabs
                                , _mwTMPTab              = tmpTab
                                , _mwTMPGroup            = _mfTMPGroup
                                , _mwTMFGroup            = _mfTMFGroup
                                , _mwTMPHeaderGroup      = _mfTMPHeaderGroup
                                , _mwTMParamDetailWindow = pdetw
                                , _mwMessageDisplay      = _mfMessageDisplay
                                , _mwMission             = _mfMission
                                , _mwDeskHeaderGroup  = _mfDeskHeaderGroup
                                }
    pure mainWindow

