module GUI.TMParamTab
  ( TMParamTabFluid(..)
  , TMParamTab
  , createTMParamTab
  )
where

import           RIO

import           Data.Colour

import           Control.Lens                   ( makeLenses
                                                , (.~)
                                                )
import           Graphics.UI.FLTK.LowLevel.FLTKHS

import           Graphics.Rendering.Chart.Backend.FLTKHS
import           Graphics.Rendering.Chart
import           Graphics.Rendering.Chart.Easy as Ch




data TMParamTabFluid = TMParamTabFluid {
  _tmParTabGroup :: Ref Group
  , _tmParGraph :: Ref Group
}


data TMParamTab = TMParamTab {
  _tmParamTab :: Ref Group
  , _tmParamGraph :: Ref Widget
}

createTMParamTab :: TMParamTabFluid -> IO TMParamTab
createTMParamTab TMParamTabFluid {..} = do
  rect    <- getRectangle _tmParGraph
  widget' <- widgetCustom rect Nothing drawChart defaultCustomWidgetFuncs
  add _tmParTabGroup widget'
  return (TMParamTab _tmParTabGroup widget')



drawChart :: Ref Widget -> IO ()
drawChart widget = do
  rectangle' <- getRectangle widget
  {-withFlClip rectangle' $ -}
  void $ renderToWidget widget chart



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

