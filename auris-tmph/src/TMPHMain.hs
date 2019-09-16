{-# LANGUAGE OverloadedStrings
    , BangPatterns
    , NoImplicitPrelude
    , RecordWildCards
    , NamedFieldPuns

#-}

module Main
where

import RIO

import Graphics.UI.FLTK.LowLevel.FLTKHS
import qualified Graphics.UI.FLTK.LowLevel.FL as FL
import Graphics.UI.FLTK.LowLevel.Fl_Types
import Graphics.UI.FLTK.LowLevel.Fl_Enumerations

import TMPH

import GUI.MainWindow
import GUI.MainWindowCallbacks
import GUI.PUSPacketTable
import GUI.Colors


ui :: IO()
ui = do
    window <- makeWindow
    mainWindow <- createMainWindow window
    setupCallbacks mainWindow
    showWidget (_mwWindow mainWindow)


createMainWindow :: MainWindowFluid -> IO MainWindow
createMainWindow MainWindowFluid {..} = do
    table <- setupTable _mfTableGroup
    mcsWindowSetColor _mfWindow
    -- mcsWidgetSetColor _mfOpenFile
    -- mcsWidgetSetColor _mfSaveFile
    mcsButtonSetColor _mfArmButton
    mcsButtonSetColor _mfGoButton
    let mainWindow = MainWindow { _mwWindow = _mfWindow
            , _mwArmButton = _mfArmButton
            , _mwGoButton = _mfGoButton
            , _mwOpenFile = _mfOpenFile
            , _mwSaveFile = _mfSaveFile
            , _mwCommandTable = table
            }
    pure mainWindow


main :: IO ()
main = ui >> FL.run >> FL.flush

replMain :: IO ()
replMain = ui >> FL.replRun




