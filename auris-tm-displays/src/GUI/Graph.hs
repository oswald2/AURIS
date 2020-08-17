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
  --, gwParent
  , gwParamSelection
  , gwGraph
  --, gwOffscreen
  )
where


import           RIO hiding ((.~))
import qualified RIO.Map                       as M
import qualified RIO.Vector                    as V
import qualified RIO.HashSet                   as HS
import           RIO.List                       ( cycle )
import qualified RIO.Text                      as T
--import           Data.Text.Short                ( ShortText )
import qualified Data.Text.Short               as ST
--import qualified Data.Text.IO                  as T
--import           Control.Lens                   ( makeLenses )


import           Data.MultiSet                  ( MultiSet )
import qualified Data.MultiSet                 as MS
import           Data.Text.Short                ( ShortText )

--import           Data.Thyme.Clock
import qualified Data.Time.Clock               as TI
import           Data.Time.Calendar

import Data.Colour.SRGB

import           Data.TM.Parameter
import           Data.TM.Value

import           General.Time


import qualified GI.Gtk as Gtk
import qualified GI.Cairo as GI
import qualified Data.GI.Base.Attributes as GI

import           Graphics.UI.FLTK.LowLevel.FLTKHS
                                               as FL
import           Graphics.UI.FLTK.LowLevel.Fl_Enumerations
--import           Graphics.UI.FLTK.LowLevel.Fl_Types
import qualified Graphics.UI.FLTK.LowLevel.FL  as FL

-- import           Graphics.Rendering.Chart.Backend.FLTKHS
import           Graphics.Rendering.Chart.Backend.Cairo
import           Graphics.Rendering.Chart as Ch
import           Graphics.Rendering.Chart.Easy as Ch
                                         hiding ( (^.) )
-- import           Graphics.Rendering.Chart.Backend.Diagrams
--import           Text.Show.Pretty


import           GUI.NameDescrTable
import           GUI.PopupMenu

import Foreign.Ptr
import Graphics.Rendering.Cairo.Internal (Render(runRender))
import Graphics.Rendering.Cairo.Types (Cairo(Cairo))


-- | The internal value of a graph. Used to get a 'Eq' and 'Ord' instance
-- across the time value (first field)
-- data GraphVal = GraphVal {
--   _graphValTime :: !TI.UTCTime
--   , _graphValValue :: !Double
-- }

type GraphVal = (TI.UTCTime, Double)


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


data TimeScaleSettings = TimeScaleSettings {
  _tssInterval :: !TI.NominalDiffTime
  , _tssOldMin :: !TI.UTCTime
  , _tssOldMax :: !TI.UTCTime
}
makeLenses ''TimeScaleSettings

timeScaleSettings now =
  let range = TI.secondsToNominalDiffTime 60
  in  TimeScaleSettings { _tssInterval = range
                        , _tssOldMin   = now
                        , _tssOldMax   = TI.addUTCTime range now
                        }

defaultTimeScaleSettings =
  let !range = TI.secondsToNominalDiffTime 60
      !defTime = TI.UTCTime (ModifiedJulianDay 0) (TI.secondsToDiffTime 0)
  in  TimeScaleSettings { _tssInterval = range
                        , _tssOldMin   = defTime
                        , _tssOldMax   = TI.addUTCTime range defTime
                        }


data Graph = Graph {
  _graphName :: !Text
  , _graphParameters :: HashSet ShortText
  , _graphData :: Map ShortText PlotVal
  , _graphTimeAxisSettings :: TimeScaleSettings
  , _graphPickFn :: Maybe (PickFn ())
  }
makeLenses ''Graph

getParameterNames :: Graph -> [ShortText]
getParameterNames g = toList (g ^. graphParameters)


emptyGraph :: Text -> Graph
emptyGraph name = Graph name HS.empty M.empty defaultTimeScaleSettings Nothing



data GraphWidget = GraphWidget {
  --_gwParent :: Ref Group
  _gwDrawingArea :: Gtk.DrawingArea
  , _gwParamSelection :: NameDescrTable
  , _gwGraph :: TVar Graph
  --, _gwOffscreen :: FlOffscreen
}
makeLenses ''GraphWidget


graphWidgetGetParamNames :: TVar Graph -> IO [ShortText]
graphWidgetGetParamNames var = getParameterNames <$> readTVarIO var

graphWidgetSetChartName :: TVar Graph -> Text -> IO ()
graphWidgetSetChartName var name = do
  let f x = x & graphName .~ name
  atomically $ modifyTVar var f



setupGraphWidget :: Gtk.Box -> Text -> NameDescrTable -> IO GraphWidget
setupGraphWidget parent title paramSelector = do

  let graph = emptyGraph title

  var       <- newTVarIO graph

  da <- Gtk.drawingAreaNew 

  Gtk.boxPackStart parent da True True 0


  let g = GraphWidget { --_gwParent         = parent
                      _gwDrawingArea    = da
                      , _gwParamSelection = paramSelector
                      , _gwGraph          = var
                      --, _gwOffscreen      = offscreen
                      }

  GI.on da #draw (drawingFunction da var)

  return g



handleMouse
  :: TVar Graph
  -> NameDescrTable
  -> Ref Widget
  -> Event
  -> IO (Either UnknownEvent ())
handleMouse var paramSelector widget Push = do
  res <- FL.eventButton3
  if res
    then do
      paramNames <- graphWidgetGetParamNames var
      let menuEntries =
            [ MenuEntry
                "Add Parameter..."
                (Just (KeyFormat "^p"))
                (Just (addParamFromSelection var paramSelector widget))
                (MenuItemFlags [MenuItemNormal])
              , MenuEntry "Set Graph Title"
                          Nothing
                          (Just (handleSetTitle var widget))
                          (MenuItemFlags [MenuItemNormal])
              , MenuEntry "Print to File"
                          Nothing
                          (Just (handlePrintToFile var))
                          (MenuItemFlags [MenuItemNormal])
              ]
              ++ map param paramNames
          param x = MenuEntry
            ("Remove Parameter/" <> ST.toText x)
            Nothing
            (Just (handleRemoveParam var widget (ST.toText x)))
            (MenuItemFlags [MenuItemNormal])


      void $ popupMenu menuEntries
      return (Right ())
    else handleWidgetBase (safeCast widget) Push
handleMouse _ _ widget Release = do
  res <- FL.eventButton3
  if res then return (Right ()) else handleWidgetBase (safeCast widget) Release
handleMouse _ _ widget event = handleWidgetBase (safeCast widget) event




handleSetTitle :: TVar Graph -> Ref Widget -> Ref MenuItem -> IO ()
handleSetTitle var widget _ = do
  res <- flInput "Set Chart Name: " Nothing
  forM_ res (graphWidgetSetChartName var)
  redraw widget


handleRemoveParam :: TVar Graph -> Ref Widget -> Text -> Ref MenuItem -> IO ()
handleRemoveParam var widget param _ = do
  void $ graphRemoveParameter' var (ST.fromText param)
  redraw widget


handlePrintToFile :: TVar Graph -> Ref MenuItem -> IO ()
handlePrintToFile gw _ = do
  graph   <- readTVarIO gw
  chooser <- nativeFileChooserNew (Just BrowseSaveFile)
  setTitle chooser "Save Chart..."
  setFilter chooser "SVG\t*.svg"
  setOptions chooser [SaveasConfirm, NewFolder, Preview, UseFilterExt]
  setPresetFile chooser $ graph ^. graphName <> ".svg"
  res <- showWidget chooser
  case res of
    NativeFileChooserPicked -> do
      name' <- getFilename chooser
      case name' of
        Just name -> do

          let options = def & fo_format .~ SVG

          void $ renderableToFile options (T.unpack name) (chart graph)
        _ -> return ()
    _ -> return ()


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


numParameters :: TVar Graph -> IO Int
numParameters var = do
  graph <- readTVarIO var
  return (HS.size (graph ^. graphParameters))


addParamFromSelection
  :: TVar Graph -> NameDescrTable -> Ref Widget -> Ref MenuItem -> IO ()
addParamFromSelection var paramSelector widget _item = do
  selItems <- getSelectedItems paramSelector
  num      <- numParameters var

  let vec    = drop (num - 1) styles
      values = zipWith (\x (l, p) -> (ST.fromText (_tableValName x), l, p))
                       selItems
                       vec

  void $ graphAddParameters' var values
  redraw widget



addParamFromSelector :: GraphWidget -> RIO.Vector TableValue -> IO ()
addParamFromSelector graphWidget table = do
  let selItems = V.toList table
  num <- numParameters (graphWidget ^. gwGraph)

  let vec    = drop (num - 1) styles
      values = zipWith (\x (l, p) -> (ST.fromText (_tableValName x), l, p))
                       selItems
                       vec
  void $ graphAddParameters graphWidget values
  --redrawGraph graphWidget


-- redrawGraph :: GraphWidget -> IO ()
-- redrawGraph gw = redraw (gw ^. gwDrawingArea)


-- | This function finally insert actual values to draw into the graph. Currently 
-- this function is a bit slow and could be optimized.
graphInsertParamValue :: GraphWidget -> RIO.Vector TMParameter -> IO ()
graphInsertParamValue gw params = do
  atomically $ do
    graph <- readTVar (gw ^. gwGraph)
    let newGraph = V.foldl insertParamValue graph params
    writeTVar (gw ^. gwGraph) newGraph
  --redrawGraph gw

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
  --redrawGraph gw
  return hs


graphRemoveParameter' :: TVar Graph -> ShortText -> IO (HashSet ShortText)
graphRemoveParameter' var name = do
  atomically $ do
    graph <- readTVar var

    let newSet   = HS.delete name (graph ^. graphParameters)
        newData  = M.delete name (graph ^. graphData)
        newGraph = graph & graphParameters .~ newSet & graphData .~ newData

    writeTVar var newGraph
    return newSet



-- | Add multiple parameters to the chart. The chart then accepts parameter values 
-- for the parameters within it's '_graphParameters' field.
graphAddParameters
  :: GraphWidget
  -> [(ShortText, Ch.LineStyle, Ch.PointStyle)]
  -> IO (HashSet ShortText)
graphAddParameters gw = graphAddParameters' (gw ^. gwGraph)


graphAddParameters'
  :: TVar Graph
  -> [(ShortText, Ch.LineStyle, Ch.PointStyle)]
  -> IO (HashSet ShortText)
graphAddParameters' var ls = do
  atomically $ do
    graph <- readTVar var

    let newGraph = foldl' insertInGraph graph ls

    writeTVar var newGraph
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
  in  case M.lookup paramName _graphData of
        Nothing -> g
        Just plotVal ->
          let val =
                --GraphVal (toUTCTime (param ^. pTime)) (toDouble (param ^. pValue))
                  (toUTCTime (param ^. pTime), toDouble (param ^. pValue))
              newSet     = MS.insert val (plotVal ^. plotValValues)
              newPlotVal = plotVal & plotValValues .~ newSet
              newMap     = M.insert paramName newPlotVal _graphData
          in  g & graphData .~ newMap


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
  where --values = map (\(GraphVal t p) -> (t, p)) . MS.toList $ _plotValValues
        values = MS.toList $ _plotValValues


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


layoutTimeAxis :: TimeScaleSettings -> LayoutAxis TI.UTCTime
layoutTimeAxis settings =
  def
    &  laxis_style
    .~ axisStyle
    &  laxis_title_style
    .  font_color
    .~ opaque white
    -- &  laxis_generate
    -- .~ timeAxisFn settings


-- timeAxisFn :: TimeScaleSettings -> AxisFn a
-- timeAxisFn settings xvals = 
--   timeValueAxis seconds seconds (formatTime defaultTimeLocale "%Ss") UnderTicks minutes (ft "%d-%b-%y %H:%M") BetweenTicks pts
--   where 
--     t0 = minimum xvals 
--     t1 = maximum xvals
--     start = truncateMinute t1 



graphLayout :: TimeScaleSettings -> Layout TI.UTCTime Double
graphLayout settings =
  let graphBgColor = sRGB24 0x29 0x3d 0x5d
  in
  layout_background
    .~ FillStyleSolid (opaque graphBgColor)
    $  layout_plot_background
    ?~ FillStyleSolid (opaque graphBgColor)
    $  layout_title_style
    .~ titleStyle
    $  layout_legend
    ?~ legendStyle
    $  layout_x_axis
    .~ layoutTimeAxis settings
    $  layout_y_axis
    .~ layoutAxis
    $  def

chart :: Graph -> Renderable ()
chart Graph {..} = toRenderable layout
 where
  plots = map (plotValToPlot . snd) $ M.toList _graphData
  layout =
    graphLayout _graphTimeAxisSettings
      &  layout_title
      .~ T.unpack _graphName
      &  layout_plots
      .~ plots


drawChart :: TVar Graph -> FlOffscreen -> Ref Widget -> IO ()
drawChart graphVar offscreen widget = do
  return ()
  -- rectangle' <- getRectangle widget
  -- graph      <- readTVarIO graphVar
  -- withFlClip rectangle' $ void $ renderToWidgetOffscreen widget
  --                                                        offscreen
  --                                                        (chart graph)


-- updateCanvas :: Renderable a -> Gtk.DrawingArea  -> IO Bool
-- updateCanvas chart canvas = do

    -- win <- Gtk.widgetGetDrawWindow canvas
    -- (width, height) <- G.widgetGetSize canvas
    -- regio <- Gtk.regionRectangle $ GE.Rectangle 0 0 width height
    -- let sz = (fromIntegral width,fromIntegral height)
    -- Gtk.drawWindowBeginPaintRegion win regio
    -- Gtk.renderWithDrawable win $ runBackend (defaultEnv bitmapAlignmentFns) (render chart sz) 
    -- Gtk.drawWindowEndPaint win
    -- return True


drawingFunction :: Gtk.DrawingArea -> TVar Graph -> GI.Context -> IO Bool 
drawingFunction drawingArea graphVar ctx = do 
  graph      <- readTVarIO graphVar
  width <- fromIntegral <$> #getAllocatedWidth drawingArea
  height <- fromIntegral <$> #getAllocatedHeight drawingArea
  
  let rndr = runBackend (defaultEnv bitmapAlignmentFns) (render (chart graph) (width, height))
  pickFn <- GI.withManagedPtr ctx $ \p -> runReaderT (runRender rndr) (Cairo (castPtr p))
  atomically $ writeTVar graphVar (graph & graphPickFn ?~ pickFn)
  return True