module GUI.TMParamTab
  ( TMParamTabFluid(..)
  , TMParamTab
  , createTMParamTab
  )
where

import           RIO
import qualified RIO.Text                      as T
import qualified Data.Text.IO                  as T
import           Data.Colour

import           Control.Lens                   ( makeLenses
                                                , (.~)
                                                )
import           Graphics.UI.FLTK.LowLevel.FLTKHS

import           Graphics.Rendering.Chart.Backend.FLTKHS
import           Graphics.Rendering.Chart
import           Graphics.Rendering.Chart.Easy as Ch

import           GUI.Colors


data TMParamTabFluid = TMParamTabFluid {
  _tmParTabGroup :: Ref Group
  , _tmParDisplaysTab :: Ref Tabs
  , _tmParGroupAND :: Ref Group
  , _tmParGroupGRD :: Ref Group
  , _tmParGroupSCD :: Ref Group
  , _tmParBrowserAND :: Ref SelectBrowser
  , _tmParBrowserGRD :: Ref SelectBrowser
  , _tmParBrowserSCD :: Ref SelectBrowser
  , _tmParGraph :: Ref Box
}


data TMParamTab = TMParamTab {
  _tmParamTab :: Ref Group
  , _tmParamDisplaysTab :: Ref Tabs
  , _tmParamGroupAND :: Ref Group
  , _tmParamGroupGRD :: Ref Group
  , _tmParamGroupSCD :: Ref Group
  , _tmParamBrowserAND :: Ref SelectBrowser
  , _tmParamBrowserGRD :: Ref SelectBrowser
  , _tmParamBrowserSCD :: Ref SelectBrowser
  , _tmParamGraph :: Ref Widget
}

createTMParamTab :: TMParamTabFluid -> IO TMParamTab
createTMParamTab TMParamTabFluid {..} = do
  rect <- getRectangle _tmParGraph
  begin _tmParTabGroup
  widget' <- widgetCustom rect Nothing drawChart defaultCustomWidgetFuncs
  end _tmParTabGroup

  mcsGroupSetColor _tmParTabGroup
  mcsTabsSetColor _tmParDisplaysTab

  mcsGroupSetColor _tmParGroupAND
  mcsGroupSetColor _tmParGroupGRD
  mcsGroupSetColor _tmParGroupSCD

  let gui = TMParamTab { _tmParamTab         = _tmParTabGroup
                       , _tmParamDisplaysTab = _tmParDisplaysTab
                       , _tmParamGroupAND    = _tmParGroupAND
                       , _tmParamGroupGRD    = _tmParGroupGRD
                       , _tmParamGroupSCD    = _tmParGroupSCD
                       , _tmParamBrowserAND  = _tmParBrowserAND
                       , _tmParamBrowserGRD  = _tmParBrowserGRD
                       , _tmParamBrowserSCD  = _tmParBrowserSCD
                       , _tmParamGraph       = widget'
                       }

  return gui



drawChart :: Ref Widget -> IO ()
drawChart widget = do
  rectangle' <- getRectangle widget
  withFlClip rectangle' $ void $ renderToWidget widget chart



chart :: Renderable ()
chart = toRenderable layout
 where
  am :: Double -> Double
  am x = (sin (x * 3.14159 / 45) + 1) / 2 * (sin (x * 3.14159 / 5))

  sinusoid1 =
    plot_lines_values
      .~ [[ (x, (am x)) | x <- [0, (0.5) .. 400] ]]
      $  plot_lines_style
      .  line_color
      .~ opaque blue
      $  plot_lines_title
      .~ "am"
      $  def

  sinusoid2 =
    plot_points_style
      .~ filledCircles 2 (opaque red)
      $  plot_points_values
      .~ [ (x, (am x)) | x <- [0, 7 .. 400] ]
      $  plot_points_title
      .~ "am points"
      $  def

  layout =
    layout_title
      .~ "Amplitude Modulation"
      $  layout_plots
      .~ [toPlot sinusoid1, toPlot sinusoid2]
      $  def

