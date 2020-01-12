{-# LANGUAGE 
  TemplateHaskell 
  , TypeApplications
#-}
module GUI.Graph
  ( Graph
  , GraphWidget
  , setupGraphWidget
  , graphInsertParamValue
  , emptyGraph
  , graphAddParameter
  , graphAddParameters
  , addParamFromSelector
  , drawChart
  , plotValName
  , plotValLineType
  , plotValPointStyle
  , plotValValues
  , graphName
  , graphParameters
  , graphData
  , gwParent
  , gwParamSelection
  , gwGraph
  )
where


import           RIO
import qualified RIO.Map                       as M
import qualified RIO.Vector                    as V
import qualified RIO.HashSet                   as HS
import           RIO.List                       ( cycle )
--import           Data.Text.Short                ( ShortText )
import qualified Data.Text.Short               as ST
--import qualified Data.Text.IO                  as T
import           Control.Lens                   ( makeLenses )


import           Data.MultiSet                  ( MultiSet )
import qualified Data.MultiSet                 as MS
import           Data.Text.Short                ( ShortText )

--import           Data.Thyme.Clock
import qualified Data.Time.Clock               as TI

import           Data.TM.Parameter
import           Data.TM.Value

import           General.Time

import           Graphics.UI.FLTK.LowLevel.FLTKHS
                                               as FL
import           Graphics.UI.FLTK.LowLevel.Fl_Enumerations
--import           Graphics.UI.FLTK.LowLevel.Fl_Types
import qualified Graphics.UI.FLTK.LowLevel.FL  as FL

import           Graphics.Rendering.Chart.Backend.FLTKHS
import           Graphics.Rendering.Chart
import           Graphics.Rendering.Chart.Easy as Ch
                                         hiding ( (^.) )

--import           Text.Show.Pretty


import           GUI.NameDescrTable
import           GUI.PopupMenu



-- | The internal value of a graph. Used to get a 'Eq' and 'Ord' instance
-- across the time value (first field)
data GraphVal = GraphVal {
  _graphValTime :: !TI.UTCTime
  , _graphValValue :: !Double
}

instance Eq GraphVal where
  g1 == g2 = _graphValTime g1 == _graphValTime g2

instance Ord GraphVal where
  compare g1 g2 = compare (_graphValTime g1) (_graphValTime g2)


-- | Defines a plot. For the parameter with the given 
-- name, the 'LineStyle' and 'PointStyle' are assigned, as well as the individual
-- plot values contained in a 'MultiSet', so that they are time-ordered
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

getParameterNames :: Graph -> [ShortText]
getParameterNames g = toList (g ^. graphParameters )


emptyGraph :: ShortText -> Graph
emptyGraph name = Graph name HS.empty M.empty


data GraphWidget = GraphWidget {
  _gwParent :: Ref Group
  , _gwDrawingArea :: Maybe (Ref Widget)
  , _gwParamSelection :: NameDescrTable
  , _gwGraph :: TVar Graph
}
makeLenses ''GraphWidget


graphWidgetGetParamNames :: GraphWidget -> IO [ShortText]
graphWidgetGetParamNames gw = getParameterNames <$> readTVarIO (gw ^. gwGraph)


setupGraphWidget :: Ref Group -> Text -> NameDescrTable -> IO GraphWidget
setupGraphWidget parent title paramSelector = do
  begin parent

  let graph = emptyGraph (ST.fromText title)

  var  <- newTVarIO graph

  rect <- getRectangle parent

  let g = GraphWidget { _gwParent         = parent
                      , _gwDrawingArea    = Nothing
                      , _gwParamSelection = paramSelector
                      , _gwGraph          = var
                      }

  widget' <- widgetCustom
    rect
    Nothing
    (drawChart var)
    defaultCustomWidgetFuncs { handleCustom = Just (handleMouse g paramSelector)
                             }

  end parent
  showWidget widget'

  let newG = g & gwDrawingArea ?~ widget'

  return newG



handleMouse
  :: GraphWidget
  -> NameDescrTable
  -> Ref Widget
  -> Event
  -> IO (Either UnknownEvent ())
handleMouse graph paramSelector widget Push = do
  res <- FL.eventButton3
  if res
    then do
      paramNames <- graphWidgetGetParamNames graph
      let menuEntries = 
            [ MenuEntry "Add Parameter..."
                        (Just (KeyFormat "^p"))
                        (Just (addParamFromSelection graph paramSelector widget))
                        (MenuItemFlags [MenuItemNormal])
            , MenuEntry "Set Graph Title" Nothing Nothing (MenuItemFlags [MenuItemNormal])
            ] ++ map param paramNames
          param x = MenuEntry ("Remove Parameter/" <> ST.toText x) Nothing Nothing (MenuItemFlags [MenuItemNormal])
          

      void $ popupMenu menuEntries
      return (Right ())
    else handleWidgetBase (safeCast widget) Push
handleMouse _ _ widget Release = do
  res <- FL.eventButton3
  if res then return (Right ()) else handleWidgetBase (safeCast widget) Release
handleMouse _ _ widget event = handleWidgetBase (safeCast widget) event


chartColors :: [AlphaColour Double]
chartColors = cycle
  [ opaque blue
  , opaque crimson
  , opaque forestgreen
  , opaque firebrick
  , opaque azure
  , opaque forestgreen
  , opaque fuchsia
  , opaque gold
  , opaque blanchedalmond
  , opaque blue
  , opaque hotpink
  ]


styles :: [(Ch.LineStyle, Ch.PointStyle)]
styles = map (\x -> (def & line_color .~ x, def)) chartColors


numParameters :: GraphWidget -> IO Int
numParameters gw = do
  graph <- readTVarIO (gw ^. gwGraph)
  return (HS.size (graph ^. graphParameters))


addParamFromSelection
  :: GraphWidget -> NameDescrTable -> Ref Widget -> Ref MenuItem -> IO ()
addParamFromSelection graphWidget paramSelector widget _item = do
  selItems <- getSelectedItems paramSelector
  num      <- numParameters graphWidget

  let vec    = drop (num - 1) styles
      values = zipWith (\x (l, p) -> (ST.fromText (_tableValName x), l, p))
                       selItems
                       vec

  void $ graphAddParameters graphWidget values
  redraw widget


addParamFromSelector :: GraphWidget -> RIO.Vector TableValue -> IO () 
addParamFromSelector graphWidget table = do 
  let selItems = V.toList table 
  num <- numParameters graphWidget 

  let vec    = drop (num - 1) styles
      values = zipWith (\x (l, p) -> (ST.fromText (_tableValName x), l, p))
                       selItems
                       vec
  void $ graphAddParameters graphWidget values 
  redrawGraph graphWidget


redrawGraph :: GraphWidget -> IO ()
redrawGraph gw = maybe (return ()) redraw (gw ^. gwDrawingArea)


-- | This function finally insert actual values to draw into the graph. Currently 
-- this function is a bit slow and could be optimized.
graphInsertParamValue :: GraphWidget -> RIO.Vector TMParameter -> IO ()
graphInsertParamValue gw params = do
  atomically $ do
    graph <- readTVar (gw ^. gwGraph)
    let newGraph = V.foldl insertParamValue graph params
    writeTVar (gw ^. gwGraph) newGraph
  redrawGraph gw

-- | Add a parameter to the chart. The chart then accepts parameter values 
-- for the parameters within it's '_graphParameters' field.
graphAddParameter
  :: GraphWidget
  -> ShortText
  -> Ch.LineStyle
  -> Ch.PointStyle
  -> IO (HashSet ShortText)
graphAddParameter gw name lineStyle pointStyle = do
  hs <- graphAddParameters gw [(name, lineStyle, pointStyle)]
  redrawGraph gw
  return hs


-- | Add multiple parameters to the chart. The chart then accepts parameter values 
-- for the parameters within it's '_graphParameters' field.
graphAddParameters
  :: GraphWidget
  -> [(ShortText, Ch.LineStyle, Ch.PointStyle)]
  -> IO (HashSet ShortText)
graphAddParameters gw ls = do
  atomically $ do
    graph <- readTVar (gw ^. gwGraph)

    let newGraph = foldl' insertInGraph graph ls

    writeTVar (gw ^. gwGraph) newGraph
    return (newGraph ^. graphParameters)

insertInGraph :: Graph -> (ShortText, Ch.LineStyle, Ch.PointStyle) -> Graph
insertInGraph graph (name, lineStyle, pointStyle) =
  let newSet   = HS.insert name (graph ^. graphParameters)
      newGraph = graph & graphParameters .~ newSet & graphData .~ newData
      -- if we already have the parameter inserted, use the old values
      newData  = M.insertWith combine
                              name
                              (PlotVal name lineStyle pointStyle MS.empty)
                              (graph ^. graphData)
      combine _ old = old
  in  newGraph



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
titleStyle =
  def
    &  font_size
    .~ 20
    &  font_weight
    .~ FontWeightBold
    &  font_color
    .~ opaque white

legendStyle :: LegendStyle
legendStyle = def & legend_label_style . font_color .~ opaque white

axisStyle :: AxisStyle
axisStyle =
  def
    &  axis_line_style
    .  line_color
    .~ opaque white
    &  axis_grid_style
    .  line_color
    .~ opaque darkslategray
    &  axis_label_style
    .  font_color
    .~ opaque white

layoutAxis :: PlotValue a => LayoutAxis a
layoutAxis =
  def
    &  laxis_style
    .~ axisStyle
    &  laxis_title_style
    .  font_color
    .~ opaque white


graphLayout :: Layout TI.UTCTime Double
graphLayout =
  layout_background
    .~ FillStyleSolid (opaque black)
    $  layout_plot_background
    ?~ FillStyleSolid (opaque black)
    $  layout_title_style
    .~ titleStyle
    $  layout_legend
    ?~ legendStyle
    $  layout_x_axis
    .~ layoutAxis
    $  layout_y_axis
    .~ layoutAxis
    $  def

chart :: Graph -> Renderable ()
chart Graph {..} = toRenderable layout
 where
  plots = map (plotValToPlot . snd) $ M.toList _graphData
  layout =
    graphLayout & layout_title .~ ST.unpack _graphName & layout_plots .~ plots


drawChart :: TVar Graph -> Ref Widget -> IO ()
drawChart var widget = do
  rectangle' <- getRectangle widget
  graph      <- readTVarIO var
  withFlClip rectangle' $ void $ renderToWidget widget (chart graph)



