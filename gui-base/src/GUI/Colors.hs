
module GUI.Colors
where

import RIO

import Graphics.UI.FLTK.LowLevel.FLTKHS
import Graphics.UI.FLTK.LowLevel.Fl_Enumerations


mcsBackground :: Color
mcsBackground = Color 0x64646400

mcsWidgetBG :: Color
mcsWidgetBG = Color 0x50505000

mcsWidgetFG :: Color
mcsWidgetFG = Color 0x7d7d7d00

mcsFontColor :: Color
mcsFontColor = Color 0xADE2E600

mcsTableBG :: Color
mcsTableBG = Color 0x7d7d7d00

mcsTableFG :: Color
mcsTableFG = Color 0xffffff00


mcsWindowSetColor :: Ref Window -> IO ()
mcsWindowSetColor w = do
    setColor w mcsBackground
    setColorWithBgSel w mcsBackground mcsWidgetFG
    setLabelcolor w mcsFontColor

mcsTextEditorSetColor :: Ref TextEditor -> IO ()
mcsTextEditorSetColor w = do
    setColor w mcsBackground
    setColorWithBgSel w mcsBackground mcsWidgetFG
    setLabelcolor w mcsFontColor


mcsMenuBarSetColor :: Ref MenuBar -> IO ()
mcsMenuBarSetColor w = do
    setColor w mcsWidgetBG
    setColorWithBgSel w mcsWidgetBG mcsWidgetFG
    setLabelcolor w mcsFontColor

mcsButtonSetColor :: Ref Button -> IO ()
mcsButtonSetColor w = do
    setColor w mcsWidgetBG
    setColorWithBgSel w mcsWidgetBG mcsWidgetFG
    setLabelcolor w mcsFontColor


mcsTabsSetColor :: Ref Tabs -> IO ()
mcsTabsSetColor w = do
    setColor w mcsWidgetBG
    setColorWithBgSel w mcsBackground mcsBackground
    setLabelcolor w mcsFontColor

mcsGroupSetColor :: Ref Group -> IO ()
mcsGroupSetColor w = do
    setColor w mcsBackground
    setColorWithBgSel w mcsBackground mcsWidgetFG
    setLabelcolor w mcsFontColor

mcsBrowserSetColor ::Ref Browser -> IO ()
mcsBrowserSetColor w = do
  setColor w mcsWidgetBG
  setColorWithBgSel w mcsWidgetBG mcsWidgetFG
