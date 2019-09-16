{-# LANGUAGE OverloadedStrings
    , BangPatterns
    , TemplateHaskell
    , NoImplicitPrelude
#-}
module GUI.MainWindow
    (
        MainWindowFluid(..)
        , MainWindow(..)
        , mwWindow
        , mwOpenFile
        , mwSaveFile
        , mwArmButton
        , mwGoButton
    )
where

import RIO

import Control.Lens (makeLenses)

import Graphics.UI.FLTK.LowLevel.FLTKHS


data MainWindowFluid = MainWindowFluid {
    _mfWindow :: Ref Window
    , _mfArmButton :: Ref Button
    , _mfGoButton :: Ref Button
    , _mfOpenFile :: Ref MenuItemBase
    , _mfSaveFile :: Ref MenuItemBase
    , _mfTableGroup :: Ref Group
    }
makeLenses ''MainWindowFluid


data MainWindow = MainWindow {
    _mwWindow :: Ref Window
    , _mwArmButton :: Ref Button
    , _mwGoButton :: Ref Button
    , _mwOpenFile :: Ref MenuItemBase
    , _mwSaveFile :: Ref MenuItemBase
    , _mwCommandTable :: Ref TableRow
    }
makeLenses ''MainWindow


