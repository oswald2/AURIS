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
  , mwTMPTile
  , mwTMFGroup
  , mwMessageDisplay
  , tmpTabButtonAdd
  , tmpTable
  , tmpModel
  , mwAddTMPacket
  , mwSetTMParameters
  )
where

import           RIO

import           Control.Lens                   ( makeLenses )

import           Graphics.UI.FLTK.LowLevel.FLTKHS

import           Model.TMPacketModel
import           Model.ScrollingTableModel

import           GUI.TMPacketTable
import           GUI.ScrollingTable
import           GUI.Colors
import           GUI.ParamDetailWindow

import           Data.PUS.TMPacket



data TMPacketTabFluid = TMPacketTabFluid {
    _tmpfTabButtonAdd :: Ref Button
    , _tmpfTabGroup :: Ref Group
}



data TMPacketTab = TMPacketTab {
    _tmpTabButtonAdd :: Ref Button
    , _tmpTable :: Ref TableRow
    , _tmpModel :: TMPacketModel
}
makeLenses ''TMPacketTab

tmpTabAddRow :: TMPacketTab -> TMPacket -> IO ()
tmpTabAddRow tab pkt = do
  addRow (tab ^. tmpTable) (tab ^. tmpModel) pkt


createTMPTab :: TMPacketTabFluid -> IO TMPacketTab
createTMPTab TMPacketTabFluid {..} = do
  model <- tableModelNew
  table <- setupTable _tmpfTabGroup model GUI.TMPacketTable.colDefinitions
  mcsGroupSetColor _tmpfTabGroup

  pure $ TMPacketTab _tmpfTabButtonAdd table model




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
    , _mfTMPTile :: Ref Tile
    , _mfTMParamGroup :: Ref Group
    , _mfMessageDisplay :: Ref Browser
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
    , _mwTMPTile :: Ref Tile
    , _mwTMFGroup :: Ref Group
    , _mwTMParamDetailWindow :: ParamDetailWindow
    , _mwMessageDisplay :: Ref Browser
    }
makeLenses ''MainWindow


mwAddTMPacket :: MainWindow -> TMPacket -> IO ()
mwAddTMPacket window pkt = do
  tmpTabAddRow (window ^. mwTMPTab) pkt

mwSetTMParameters :: MainWindow -> TMPacket -> IO ()
mwSetTMParameters window pkt = do
  parDetWinSetValues (window ^. mwTMParamDetailWindow) pkt



createMainWindow :: MainWindowFluid -> ParamDetailWindowFluid -> IO MainWindow
createMainWindow MainWindowFluid {..} paramDetailWindow = do
  tmpTab  <- createTMPTab _mfTMPTab
  mcsWindowSetColor _mfWindow
  mcsTabsSetColor _mfTabs
  mcsGroupSetColor _mfTMPGroup
  mcsGroupSetColor _mfTMFGroup
  mcsGroupSetColor _mfTMPHeaderGroup
  mcsBrowserSetColor _mfMessageDisplay

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
                              , _mwTMPTile             = _mfTMPTile
                              , _mwTMParamDetailWindow = pdetw
                              , _mwMessageDisplay      = _mfMessageDisplay
                              }
  pure mainWindow

