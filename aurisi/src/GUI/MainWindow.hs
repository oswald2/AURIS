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
  , mwWindow
  , mwOpenFile
  , mwSaveFile
  , mwProgress
  , mwTabs
  , mwTMPTab
  , tmpTabButtonAdd
  , tmpTable
  , tmpModel
  , createTMPTab
  , mwAddTMPacket
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
    }


data MainWindow = MainWindow {
    _mwWindow :: Ref Window
    , _mwOpenFile :: Ref MenuItemBase
    , _mwSaveFile :: Ref MenuItemBase
    , _mwProgress :: Ref Progress
    , _mwTabs :: Ref Tabs
    , _mwTMPTab :: TMPacketTab
    }
makeLenses ''MainWindow


mwAddTMPacket :: MainWindow -> TMPacket -> IO ()
mwAddTMPacket window pkt = do
  tmpTabAddRow (window ^. mwTMPTab) pkt
