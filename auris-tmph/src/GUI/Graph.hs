{-# LANGUAGE TemplateHaskell #-}
module GUI.Graph
  ( Graph
  )
where


import           RIO
import qualified RIO.Map                       as M
import           Data.Text.Short                ( ShortText )
import qualified Data.Text.Short               as ST
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


data PlotVal = PlotVal {
  _plotValName :: !ShortText
  , _plotValLineType :: !Ch.LineStyle
  , _plotValPointStyle :: !Ch.PointStyle
  , _plotValValues :: MultiSet GraphVal
}
makeLenses ''PlotVal

data Graph = Graph {
  _graphWidget :: Ref Widget
  , _graphRect :: !FL.Rectangle
  , _graphName :: !ShortText
  , _graphData :: Map ShortText PlotVal
  }
makeLenses ''Graph

graphInsertParamValue :: Graph -> TMParameter -> Graph
graphInsertParamValue g@Graph {..} param =
  case M.lookup (param ^. pName) _graphData of
    Nothing -> g
    Just plotVal ->
      let val        = GraphVal (param ^. pTime) (toDouble (param ^. pValue))
          newSet     = MS.insert val (plotVal ^. plotValValues)
          newPlotVal = plotVal & plotValValues .~ newSet
          newMap     = M.insert (param ^. pName) newPlotVal _graphData
      in  g & graphData .~ newMap


plotValToPlot :: PlotVal -> Plot SunTime Double
plotValToPlot PlotVal {..} =
  toPlot
    $  plot_lines_values
    .~ [values]
    $  plot_lines_style
    .~ _plotValLineType
    -- $  plot_points_style .~ _plotValPointStyle
    $  plot_lines_title
    .~ ST.unpack _plotValName
    $  def
  where values = map (\(GraphVal t p) -> (t, p)) . MS.toList $ _plotValValues



-- valuesToPlot :: Graph -> [Plot x y]
-- valuesToPlot graph = 
--   let conv (GraphVal t p) = (t, p)
--       map (conf . MS.toList . snd) $ M.toList (graph ^. graphData)


drawChart :: Graph -> Ref Widget -> IO ()
drawChart graph widget = do
  rectangle' <- getRectangle widget
  withFlClip rectangle' $ void $ renderToWidget widget (chart graph)



chart :: Graph -> Renderable ()
chart Graph {..} = toRenderable layout
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
      .~ ST.unpack _graphName
      $  layout_plots
      .~ [toPlot sinusoid1, toPlot sinusoid2]
      $  def

