
module GUI.Colors where

import           RIO

import           Graphics.UI.FLTK.LowLevel.FLTKHS
import           Graphics.UI.FLTK.LowLevel.Fl_Enumerations

import           GI.Gdk.Structs.RGBA
import qualified GI.Gdk.Structs.Color as Gdk

import           System.IO.Unsafe

-- mcsBackground :: Color
-- mcsBackground = Color 0x64646400

-- mcsWidgetBG :: Color
-- mcsWidgetBG = Color 0x50505000

-- mcsWidgetBGGroup :: Color
-- mcsWidgetBGGroup = Color 0x46464600

-- mcsWidgetFG :: Color
-- mcsWidgetFG = Color 0x7d7d7d00

-- mcsFontColor :: Color
-- mcsFontColor = Color 0xADE2E600

-- mcsEmphColor :: Color
-- mcsEmphColor = Color 0xD9D9A800


-- mcsTableBG :: Color
-- mcsTableBG = Color 0x7d7d7d00

-- mcsTableFG :: Color
-- mcsTableFG = Color 0xffffff00

-- mcsTableSelectionColor :: Color
-- mcsTableSelectionColor = Color 0x0000aa00

-- mcsWhite :: Color
-- mcsWhite = Color 0xffffff00

-- mcsBlack :: Color
-- mcsBlack = Color 0

-- mcsRed :: Color
-- mcsRed = Color 0xff000000

-- mcsGreen :: Color
-- mcsGreen = Color 0x00CC0000

-- mcsYellow :: Color
-- mcsYellow = Color 0xffff0000





mcsBackground :: Color
mcsBackground = Color 0x1d263800

mcsWidgetBG :: Color
mcsWidgetBG = Color 0x293d5d00

mcsWidgetBGGroup :: Color
mcsWidgetBGGroup = Color 0x1c293f00

mcsWidgetFG :: Color
mcsWidgetFG = Color 0x4d979f00

mcsFontColor :: Color
mcsFontColor = Color 0x70AFDF00

mcsEmphColor :: Color
mcsEmphColor = Color 0xD9D9A800


mcsTableBG :: Color
mcsTableBG = Color 0x1c293f00

mcsTableFG :: Color
mcsTableFG = Color 0xafb7c400

mcsTableSelectionColor :: Color
mcsTableSelectionColor = Color 0x4a6ba500

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

{-# NOINLINE paleYellow #-}
paleYellow :: RGBA
paleYellow = unsafePerformIO $ do
  col <- newZeroRGBA
  void $ rGBAParse col "#ffff00"
  return col

{-# NOINLINE black #-}
black :: RGBA
black = unsafePerformIO $ do
  col <- newZeroRGBA
  void $ rGBAParse col "#000000"
  return col

{-# NOINLINE red #-}
red :: RGBA
red = unsafePerformIO $ do
  col <- newZeroRGBA
  void $ rGBAParse col "#ff0000"
  return col

{-# NOINLINE green #-}
green :: RGBA
green = unsafePerformIO $ do
  col <- newZeroRGBA
  res <- rGBAParse col "#00cc00"
  unless res $ error "Could not parse green!"
  return col


{-# NOINLINE white #-}
white :: RGBA
white = unsafePerformIO $ do
  col <- newZeroRGBA
  void $ rGBAParse col "#ffffff"
  return col


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

mcsSysMenuBarSetColor :: Ref SysMenuBar -> IO ()
mcsSysMenuBarSetColor w = do
  setColor w mcsWidgetBG
  setColorWithBgSel w mcsWidgetBG mcsWidgetFG
  setLabelcolor w mcsFontColor


mcsButtonSetColor :: Ref Button -> IO ()
mcsButtonSetColor w = do
  setColor w mcsWidgetBG
  setColorWithBgSel w mcsWidgetBG mcsWidgetFG
  setLabelcolor w mcsFontColor

mcsRoundButtonSetColor :: Ref RoundButton -> IO ()
mcsRoundButtonSetColor w = do
  setColor w mcsWidgetBG
  setColorWithBgSel w mcsWidgetBG mcsYellow
  setLabelcolor w mcsFontColor


mcsLightButtonSetColor :: Ref LightButton -> IO ()
mcsLightButtonSetColor w = do
  setColor w mcsWidgetBG
  setColorWithBgSel w mcsWidgetBG mcsYellow
  setLabelcolor w mcsFontColor


mcsTabsSetColor :: Ref Tabs -> IO ()
mcsTabsSetColor w = do
  setColor w mcsWidgetBG
  setColorWithBgSel w mcsTableBG mcsTableBG
  setLabelcolor w mcsFontColor

mcsGroupSetColor :: Ref Group -> IO ()
mcsGroupSetColor w = do
  setColor w mcsBackground
  setColorWithBgSel w mcsBackground mcsWidgetBG
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
  setColorWithBgSel w mcsWidgetBG mcsTableSelectionColor
  setTextcolor w mcsFontColor


mcsTableSetColor :: Ref TableRow -> IO ()
mcsTableSetColor w = do
  setColor w mcsTableBG
  setSelectionColor w mcsTableSelectionColor

mcsInputSetColor :: Ref Input -> IO ()
mcsInputSetColor w = do
  setColor w mcsWidgetBG
  setSelectionColor w mcsTableSelectionColor
  setTextcolor w mcsTableFG

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
