{-# LANGUAGE TemplateHaskell #-}
module GUI.Graph
  ( Graph
  , graphInsertParamValue
  , emptyGraph
  , graphAddParameter
  , drawChart
  )
where


import           RIO
import qualified RIO.Map                       as M
import qualified RIO.Vector                    as V
import qualified RIO.HashSet                   as HS
import           Data.Text.Short                ( ShortText )
import qualified Data.Text.Short               as ST
import qualified Data.Text.IO                  as T
import           Control.Lens                   ( makeLenses )


import           Data.MultiSet                  ( MultiSet )
import qualified Data.MultiSet                 as MS
import           Data.Text.Short                ( ShortText )

import           Data.Thyme.Clock
import qualified Data.Time.Clock               as TI

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
  _graphValTime :: !TI.UTCTime
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
  _graphName :: !ShortText
  , _graphParameters :: HashSet ShortText
  , _graphData :: Map ShortText PlotVal
  }
makeLenses ''Graph



emptyGraph :: ShortText -> Graph
emptyGraph name = Graph name HS.empty M.empty



graphInsertParamValue :: TVar Graph -> RIO.Vector TMParameter -> IO ()
graphInsertParamValue var params = do
  atomically $ do
    graph <- readTVar var
    let newGraph = V.foldl insertParamValue graph params
    writeTVar var newGraph

-- | Add a parameter to the chart. The chart then accepts parameter values 
-- for the parameters within it's '_graphParameters' field.
graphAddParameter
  :: TVar Graph
  -> ShortText
  -> Ch.LineStyle
  -> Ch.PointStyle
  -> IO (HashSet ShortText)
graphAddParameter var name lineStyle pointStyle = do
  atomically $ do
    graph <- readTVar var
    let newSet   = HS.insert name (graph ^. graphParameters)
        newGraph = graph & graphParameters .~ newSet & graphData .~ newData
        newData  = M.insert name
                            (PlotVal name lineStyle pointStyle MS.empty)
                            (graph ^. graphData)
    writeTVar var newGraph
    return newSet



insertParamValue :: Graph -> TMParameter -> Graph
insertParamValue g@Graph {..} param =
  let paramName = param ^. pName
  in
    case M.lookup paramName _graphData of
      Nothing -> g
      Just plotVal ->
        let
          val =
            GraphVal (toUTCTime (param ^. pTime)) (toDouble (param ^. pValue))
          newSet     = MS.insert val (plotVal ^. plotValValues)
          newPlotVal = plotVal & plotValValues .~ newSet
          newMap     = M.insert paramName newPlotVal _graphData
        in
          g & graphData .~ newMap


plotValToPlot :: PlotVal -> Plot TI.UTCTime Double
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


titleStyle :: FontStyle
titleStyle = def 
  & font_size .~ 20 
  & font_weight .~ FontWeightBold 
  & font_color .~ opaque white

legendStyle :: LegendStyle 
legendStyle = def & legend_label_style . font_color .~ opaque white

axisStyle :: AxisStyle 
axisStyle = def 
  & axis_line_style . line_color .~ opaque white
  & axis_grid_style . line_color .~ opaque darkslategray 
  & axis_label_style . font_color .~ opaque white

layoutAxis :: PlotValue a => LayoutAxis a 
layoutAxis = def 
  & laxis_style .~ axisStyle 
  & laxis_title_style . font_color .~ opaque white


graphLayout :: Layout TI.UTCTime Double 
graphLayout = 
    layout_background
      .~ FillStyleSolid (opaque black)
      $  layout_plot_background
      ?~ FillStyleSolid (opaque black)
      $ layout_title_style .~ titleStyle
      $ layout_legend ?~ legendStyle 
      $ layout_x_axis .~ layoutAxis 
      $ layout_y_axis .~ layoutAxis 
      $  def

chart :: Graph -> Renderable ()
chart Graph {..} = toRenderable layout
 where
  plots = map (plotValToPlot . snd) $ M.toList _graphData
  layout = graphLayout 
    & layout_title .~ ST.unpack _graphName
    & layout_plots .~ plots 


drawChart :: TVar Graph -> Ref Widget -> IO ()
drawChart var widget = do
  rectangle' <- getRectangle widget
  graph      <- readTVarIO var
  withFlClip rectangle' $ void $ renderToWidget widget (chart graph)



