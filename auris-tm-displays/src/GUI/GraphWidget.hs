{-# LANGUAGE 
  TemplateHaskell 
  , TypeApplications
#-}
module GUI.GraphWidget
  ( GraphWidget
  , setupGraphWidget
  , graphWidgetInsertParamValue
  , emptyGraph
  , graphWidgetAddParameter
  , graphWidgetAddParameters
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
import           Graphics.Rendering.Chart.Backend.GI.Cairo
import           Graphics.Rendering.Chart as Ch
import           Graphics.Rendering.Chart.Easy as Ch
                                         hiding ( (^.) )
-- import           Graphics.Rendering.Chart.Backend.Diagrams
--import           Text.Show.Pretty


import           GUI.NameDescrTable
import           GUI.PopupMenu
import           GUI.Chart

import Foreign.Ptr
import GI.Cairo.Render.Connector





data GraphWidget = GraphWidget {
  _gwParent :: !Gtk.Box
  , _gwDrawingArea :: !Gtk.DrawingArea
  , _gwParamSelection :: !NameDescrTable
  , _gwGraph :: TVar Graph
  , _gwPickFn :: TVar (Maybe (PickFn ()))
}
makeLenses ''GraphWidget


graphWidgetGetParamNames :: GraphWidget -> IO [ShortText]
graphWidgetGetParamNames w = graphGetParameterNames <$> readTVarIO (w ^. gwGraph)

graphWidgetSetChartName :: GraphWidget -> Text -> IO ()
graphWidgetSetChartName w name = do
  let f x = x & graphName .~ name
  atomically $ modifyTVar (w ^. gwGraph) f



setupGraphWidget :: Gtk.Box -> Text -> NameDescrTable -> IO GraphWidget
setupGraphWidget parent title paramSelector = do
  let graph = emptyGraph title
  var       <- newTVarIO graph
  var2      <- newTVarIO Nothing 
  da <- Gtk.drawingAreaNew 
  Gtk.boxPackStart parent da True True 0

  let g = GraphWidget { _gwParent         = parent
                      , _gwDrawingArea    = da
                      , _gwParamSelection = paramSelector
                      , _gwGraph          = var
                      , _gwPickFn         = var2 
                      }

  void $ GI.on da #draw (drawingFunction g)
  return g



-- handleMouse
--   :: TVar Graph
--   -> NameDescrTable
--   -> Ref Widget
--   -> Event
--   -> IO (Either UnknownEvent ())
-- handleMouse var paramSelector widget Push = do
--   res <- FL.eventButton3
--   if res
--     then do
--       paramNames <- graphWidgetGetParamNames var
--       let menuEntries =
--             [ MenuEntry
--                 "Add Parameter..."
--                 (Just (KeyFormat "^p"))
--                 (Just (addParamFromSelection var paramSelector widget))
--                 (MenuItemFlags [MenuItemNormal])
--               , MenuEntry "Set Graph Title"
--                           Nothing
--                           (Just (handleSetTitle var widget))
--                           (MenuItemFlags [MenuItemNormal])
--               , MenuEntry "Print to File"
--                           Nothing
--                           (Just (handlePrintToFile var))
--                           (MenuItemFlags [MenuItemNormal])
--               ]
--               ++ map param paramNames
--           param x = MenuEntry
--             ("Remove Parameter/" <> ST.toText x)
--             Nothing
--             (Just (handleRemoveParam var widget (ST.toText x)))
--             (MenuItemFlags [MenuItemNormal])


--       void $ popupMenu menuEntries
--       return (Right ())
--     else handleWidgetBase (safeCast widget) Push
-- handleMouse _ _ widget Release = do
--   res <- FL.eventButton3
--   if res then return (Right ()) else handleWidgetBase (safeCast widget) Release
-- handleMouse _ _ widget event = handleWidgetBase (safeCast widget) event




-- handleSetTitle :: TVar Graph -> Ref Widget -> Ref MenuItem -> IO ()
-- handleSetTitle var widget _ = do
--   res <- flInput "Set Chart Name: " Nothing
--   forM_ res (graphWidgetSetChartName var)
--   -- redraw widget


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


numParameters :: TVar Graph -> IO Int
numParameters var = do
  graph <- readTVarIO var
  return (HS.size (graph ^. graphParameters))


addParamFromSelection
  :: TVar Graph -> NameDescrTable -> Ref Widget -> Ref MenuItem -> IO ()
addParamFromSelection var paramSelector widget _item = do
  selItems <- getSelectedItems paramSelector
  num      <- numParameters var

  let vec    = drop (num - 1) chartStyles
      values = zipWith (\x (l, p) -> (ST.fromText (_tableValName x), l, p))
                       selItems
                       vec

  void $ graphAddParameters' var values
  redraw widget



addParamFromSelector :: GraphWidget -> RIO.Vector TableValue -> IO ()
addParamFromSelector graphWidget table = do
  let selItems = V.toList table
  num <- numParameters (graphWidget ^. gwGraph)

  let vec    = drop (num - 1) chartStyles
      values = zipWith (\x (l, p) -> (ST.fromText (_tableValName x), l, p))
                       selItems
                       vec
  void $ graphWidgetAddParameters graphWidget values
  redrawGraph graphWidget


redrawGraph :: GraphWidget -> IO ()
redrawGraph gw = do 
  let da = gw ^. gwDrawingArea
  Gtk.widgetQueueDraw da 


-- | This function finally insert actual values to draw into the graph. Currently 
-- this function is a bit slow and could be optimized.
graphWidgetInsertParamValue :: GraphWidget -> RIO.Vector TMParameter -> IO ()
graphWidgetInsertParamValue gw params = do
  atomically $ do
    graph <- readTVar (gw ^. gwGraph)
    let newGraph = V.foldl graphInsertParamValue graph params
    writeTVar (gw ^. gwGraph) newGraph
  redrawGraph gw

-- | Add a parameter to the chart. The chart then accepts parameter values 
-- for the parameters within it's '_graphParameters' field.
graphWidgetAddParameter
  :: GraphWidget
  -> ShortText
  -> Ch.LineStyle
  -> Ch.PointStyle
  -> IO (HashSet ShortText)
graphWidgetAddParameter gw name lineStyle pointStyle = do
  hs <- graphWidgetAddParameters gw [(name, lineStyle, pointStyle)]
  redrawGraph gw
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
graphWidgetAddParameters
  :: GraphWidget
  -> [(ShortText, Ch.LineStyle, Ch.PointStyle)]
  -> IO (HashSet ShortText)
graphWidgetAddParameters gw params = do 
  res <- graphAddParameters' (gw ^. gwGraph) params 
  redrawGraph gw
  return res


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






chart :: Graph -> Renderable ()
chart Graph {..} = toRenderable layout
 where
  plots = map (plotValToPlot . snd) $ M.toList _graphData
  layout =
    chartLayout _graphTimeAxisSettings
      &  layout_title
      .~ T.unpack _graphName
      &  layout_plots
      .~ plots


drawChart :: TVar Graph -> FlOffscreen -> Ref Widget -> IO ()
drawChart _graphVar _offscreen _widget = do
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


drawingFunction :: GraphWidget -> GI.Context -> IO Bool 
drawingFunction w ctx = do 
  graph      <- readTVarIO (w ^. gwGraph)
  let drawingArea = w ^. gwDrawingArea
  width <- fromIntegral <$> #getAllocatedWidth drawingArea
  height <- fromIntegral <$> #getAllocatedHeight drawingArea
  
  let rndr = runBackend (defaultEnv bitmapAlignmentFns) (render (chart graph) (width, height))
  pickFn <- renderWithContext rndr ctx 
  atomically $ writeTVar (w ^. gwPickFn) (Just pickFn)
  return True