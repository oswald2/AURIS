
module GUI.Colors where

import           RIO

import           Graphics.UI.FLTK.LowLevel.FLTKHS
import           Graphics.UI.FLTK.LowLevel.Fl_Enumerations


mcsBackground :: Color
mcsBackground = Color 0x64646400

mcsWidgetBG :: Color
mcsWidgetBG = Color 0x50505000

mcsWidgetBGGroup :: Color
mcsWidgetBGGroup = Color 0x46464600

mcsWidgetFG :: Color
mcsWidgetFG = Color 0x7d7d7d00

mcsFontColor :: Color
mcsFontColor = Color 0xADE2E600

mcsEmphColor :: Color
mcsEmphColor = Color 0xD9D9A800


mcsTableBG :: Color
mcsTableBG = Color 0x7d7d7d00

mcsTableFG :: Color
mcsTableFG = Color 0xffffff00

mcsTableSelectionColor :: Color
mcsTableSelectionColor = Color 0x0000aa00

mcsWhite :: Color
mcsWhite = Color 0xffffff00

mcsBlack :: Color
mcsBlack = Color 0

mcsRed :: Color
mcsRed = Color 0xff000000

mcsGreen :: Color
mcsGreen = Color 0x00CC0000

mcsYellow :: Color
mcsYellow = Color 0xffff0000


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

mcsHeaderGroupSetColor :: Ref Group -> IO ()
mcsHeaderGroupSetColor w = do
  setColor w mcsWidgetBG
  setColorWithBgSel w mcsWidgetBG mcsFontColor
  setLabelcolor w mcsFontColor

mcsGroupingSetColor :: Ref Group -> IO ()
mcsGroupingSetColor w = do
  setColor w mcsWidgetBGGroup
  setColorWithBgSel w mcsWidgetBGGroup mcsFontColor
  setLabelcolor w mcsFontColor


mcsBrowserSetColor :: Ref Browser -> IO ()
mcsBrowserSetColor w = do
  setColor w mcsWidgetBG
  setColorWithBgSel w mcsWidgetBG mcsWidgetFG


mcsTableSetColor :: Ref TableRow -> IO ()
mcsTableSetColor w = do
  setColor w mcsTableBG
  setSelectionColor w mcsTableSelectionColor


mcsOutputSetColor :: Ref Output -> IO ()
mcsOutputSetColor w = do
  setColor w mcsWidgetBG
  setColorWithBgSel w mcsWidgetBG mcsWidgetFG
  setLabelcolor w mcsFontColor
  setTextcolor w mcsEmphColor

mcsLabelSetColor :: Ref Output -> IO ()
mcsLabelSetColor w = do
  setColor w mcsWidgetBG
  setColorWithBgSel w mcsWidgetBG mcsWidgetFG
  setLabelcolor w mcsFontColor
  setTextcolor w mcsEmphColor

mcsScrolledSetColor :: Ref Scrolled -> IO ()
mcsScrolledSetColor w = do
  setColor w mcsWidgetBG
  setColorWithBgSel w mcsWidgetBG mcsWidgetFG

mcsTextDisplaySetColor :: Ref TextDisplay -> IO ()
mcsTextDisplaySetColor w = do
  setColor w mcsTableBG
  setColorWithBgSel w mcsTableBG mcsTableFG
  setTextcolor w mcsTableFG

mcsProgressSetColor :: Ref Progress -> IO ()
mcsProgressSetColor w = do
  setColor w mcsBackground
  setColorWithBgSel w mcsBackground mcsTableSelectionColor


mcsBoxLabel :: Ref Box -> IO ()
mcsBoxLabel w = do
  setLabelcolor w mcsFontColor

mcsBoxTime :: Ref Box -> IO ()
mcsBoxTime w = do
  setColor w mcsWidgetBGGroup
  setLabelcolor w mcsEmphColor

mcsBoxAlarm :: Ref Box -> Text -> IO ()
mcsBoxAlarm w t = do
  setLabel w t
  setColor w mcsRed
  setLabelcolor w mcsWhite

mcsBoxGreen :: Ref Box -> Text -> IO ()
mcsBoxGreen w t = do
  setLabel w t
  setColor w mcsGreen
  setLabelcolor w mcsBlack

mcsBoxWarn :: Ref Box -> Text -> IO ()
mcsBoxWarn w t = do
  setLabel w t
  setColor w mcsYellow
  setLabelcolor w mcsBlack