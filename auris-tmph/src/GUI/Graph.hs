{-# LANGUAGE TemplateHaskell #-}
module GUI.Graph
  ( Graph
  )
where


import           RIO
import qualified RIO.HashMap                   as HM

import           Control.Lens                   ( makeLenses )


import           Data.MultiSet                  ( MultiSet )
import qualified Data.MultiSet                 as MS
import           Data.Text.Short                ( ShortText )

import           Data.Thyme.Clock

import           Data.TM.Parameter
import           Data.TM.Value

import           General.Time

import           Graphics.UI.FLTK.LowLevel.FLTKHS
                                               as FL

import           Graphics.Rendering.Chart.Backend.FLTKHS
import           Graphics.Rendering.Chart
import           Graphics.Rendering.Chart.Easy as Ch
                                         hiding ( (^.) )



data GraphVal = GraphVal {
  _graphValTime :: !SunTime
  , _graphValValue :: !Double
}

instance Eq GraphVal where
  g1 == g2 = _graphValTime g1 == _graphValTime g2

instance Ord GraphVal where
  compare g1 g2 = compare (_graphValTime g1) (_graphValTime g2)



data Graph = Graph {
  _graphWidget :: Ref Widget
  , _graphRect :: !FL.Rectangle
  , _graphData :: HashMap ShortText (MultiSet GraphVal)
  }
makeLenses ''Graph

graphInsertParamValue :: Graph -> TMParameter -> Graph
graphInsertParamValue g@Graph {..} param =
  case HM.lookup (param ^. pName) _graphData of
    Nothing -> g
    Just set ->
      let val    = GraphVal (param ^. pTime) (toDouble (param ^. pValue))
          newSet = MS.insert val set
          newHM  = HM.insert (param ^. pName) newSet _graphData
      in  g & graphData .~ newHM

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

