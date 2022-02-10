{-# LANGUAGE 
  TemplateHaskell 
  , TypeApplications
#-}
module GUI.GraphWidget
    ( GraphWidget
    , setupGraphWidget
    , setupGraphPropertiesDialog
    , graphWidgetInsertParamValue
    , graphWidgetClearValues
    , emptyGraph
    , graphWidgetAddParameter
    , graphWidgetAddParameters
    , graphWidgetRemoveParameter
    , graphWidgetSetChartName
    , graphWidgetGetParamNames
    , graphWidgetSaveToSVG
    , graphWidgetSaveToSVGAction
    , graphWidgetDestroy
    , graphWidgetShow
    , graphWidgetHide
    , addParamFromSelector
    , plotValName
    , plotValLineType
    , plotValPointStyle
    , plotValValues
    , graphParameters
    , graphData
    , gwParamSelection
    , gwGraph
    , gwParent
    , GraphPropertiesDialog
    , defaultGraphProperties
    , GraphProperties(..)
    ) where


import qualified Data.Text.Short               as ST
import           RIO                     hiding ( (.~) )
import qualified RIO.HashSet                   as HS
import qualified RIO.Map                       as M
import           RIO.Partial                    ( toEnum )
import qualified RIO.Text                      as T
import qualified RIO.Vector                    as V

import           Data.Text.Short                ( ShortText )

import           Data.TM.Parameter


import qualified GI.Cairo                      as GI
import qualified GI.Gdk.Flags                  as GI
import qualified GI.Gdk.Structs.EventButton    as GI
import qualified GI.Gtk                        as Gtk

import           Graphics.Rendering.Chart      as Ch
import           Graphics.Rendering.Chart.Backend.GI.Cairo
import           Graphics.Rendering.Chart.Easy as Ch
                                         hiding ( (^.) )

import           GUI.Chart
import           GUI.NameDescrTable

import qualified Data.Time.Clock               as TI
import           GI.Cairo.Render.Connector      ( renderWithContext )
import           GI.Gtk.Objects                 ( builderNewFromResource )
import           GUI.Utils




data GraphPropertiesDialog = GraphPropertiesDialog
    { _gpdDialog  :: !Gtk.Dialog
    , _gpdTitle   :: !Gtk.Entry
    , _gpdBGColor :: !Gtk.ColorButton
    }
makeLenses ''GraphPropertiesDialog



data GraphWidget = GraphWidget
    { _gwWindow           :: !Gtk.Window
    , _gwParent           :: !Gtk.Box
    , _gwChartBox         :: !Gtk.Box
    , _gwDrawingArea      :: !Gtk.DrawingArea
    , _gwParamSelection   :: !NameDescrTable
    , _gwTimeRange        :: !Gtk.SpinButton
    , _gwGraph            :: TVar Graph
    , _gwPickFn           :: TVar (Maybe (PickFn ()))
    , _gwPropertiesDialog :: !GraphPropertiesDialog
    }
makeLenses ''GraphWidget


graphWidgetGetParamNames :: GraphWidget -> IO [ShortText]
graphWidgetGetParamNames w =
    graphGetParameterNames <$> readTVarIO (w ^. gwGraph)

graphWidgetSetChartName :: GraphWidget -> Text -> IO ()
graphWidgetSetChartName w name = do
    atomically $ modifyTVar (w ^. gwGraph) $ \g -> g & graphProperties . gpTitle .~ name


graphWidgetShow :: GraphWidget -> IO ()
graphWidgetShow gw = do
    Gtk.widgetShow (gw ^. gwDrawingArea)

graphWidgetHide :: GraphWidget -> IO ()
graphWidgetHide gw = do
    Gtk.widgetHide (gw ^. gwDrawingArea)



graphWidgetDestroy :: GraphWidget -> IO ()
graphWidgetDestroy gw = Gtk.widgetDestroy (gw ^. gwChartBox)

setupGraphWidget
    :: Gtk.Window
    -> Gtk.Box
    -> Text
    -> NameDescrTable
    -> GraphPropertiesDialog
    -> IO GraphWidget
setupGraphWidget window parent title paramSelector dialog = do
    let graph = emptyGraph title
    var            <- newTVarIO graph
    var2           <- newTVarIO Nothing

    builder        <- Gtk.builderNewFromResource "/auris/data/ChartWidget.glade"
    da             <- getObject builder "drawingAreaChart" Gtk.DrawingArea
    chartBox       <- getObject builder "boxChart" Gtk.Box
    entryTimeRange <- getObject builder "spinbuttonGraphRange" Gtk.SpinButton

    Gtk.widgetAddEvents da [GI.EventMaskButtonPressMask]

    Gtk.boxPackStart parent chartBox True True 0

    let g = GraphWidget { _gwWindow           = window
                        , _gwParent           = parent
                        , _gwChartBox         = chartBox
                        , _gwDrawingArea      = da
                        , _gwParamSelection   = paramSelector
                        , _gwTimeRange        = entryTimeRange
                        , _gwGraph            = var
                        , _gwPickFn           = var2
                        , _gwPropertiesDialog = dialog
                        }

    void $ GI.on da #draw (drawingFunction g)

    void $ GI.on da #buttonPressEvent $ \ev -> do
        bt <- GI.getEventButtonButton ev
        case bt of
            3 -> do
                m <- createPopupMenu g
                Gtk.menuPopupAtPointer m Nothing
                return True
            _ -> return False

    void $ GI.on entryTimeRange #valueChanged $ do
        val <- Gtk.spinButtonGetValue entryTimeRange
        atomically
            $ modifyTVar' var (\gr -> graphSetTimeRange gr (realToFrac val))

    return g



graphWidgetSaveToSVGAction :: GraphWidget -> IO ()
graphWidgetSaveToSVGAction gw = do
    fc <- Gtk.fileChooserNativeNew (Just "Save SVG...")
                                   (Just (gw ^. gwWindow))
                                   Gtk.FileChooserActionSave
                                   Nothing
                                   Nothing

    filt <- Gtk.fileFilterNew
    Gtk.fileFilterAddPattern filt "*.svg"

    Gtk.fileChooserAddFilter fc filt
    Gtk.fileChooserSetDoOverwriteConfirmation fc True
    Gtk.nativeDialogSetTitle fc "Save SVG..."
    Gtk.nativeDialogSetTransientFor fc (Just (gw ^. gwWindow))
    res <- Gtk.nativeDialogRun fc
    case toEnum (fromIntegral res) of
        Gtk.ResponseTypeAccept -> do
            fileName <- Gtk.fileChooserGetFilename fc
            forM_ fileName (graphWidgetSaveToSVG gw)
        _ -> return ()

graphWidgetSaveToSVG :: GraphWidget -> FilePath -> IO ()
graphWidgetSaveToSVG gw fn = do
    graph <- readTVarIO (gw ^. gwGraph)
    let options = def & fo_format .~ SVG
    void $ renderableToFile options fn (chart graph)

numParameters :: TVar Graph -> IO Int
numParameters var = do
    graph <- readTVarIO var
    return (HS.size (graph ^. graphParameters))



addParamFromSelector :: GraphWidget -> RIO.Vector TableValue -> IO ()
addParamFromSelector graphWidget table = do
    let selItems = V.toList table
    num <- numParameters (graphWidget ^. gwGraph)

    let vec    = drop (num - 1) chartStyles
        values = zipWith
            (\x (l, p) -> (ST.fromText (_tableValName x), l, p))
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
    now <- TI.getCurrentTime
    atomically $ do
        graph <- readTVar (gw ^. gwGraph)
        let !newGraph = graphInsertParamValue now graph (V.toList params)
        writeTVar (gw ^. gwGraph) newGraph
    redrawGraph gw


graphWidgetClearValues :: GraphWidget -> IO ()
graphWidgetClearValues gw = do
    atomically $ do
        graph <- readTVar (gw ^. gwGraph)
        writeTVar (gw ^. gwGraph) (graphClearValues graph)
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



graphWidgetRemoveParameter :: GraphWidget -> ShortText -> IO ()
graphWidgetRemoveParameter gw name = do
    atomically $ do
        graph <- readTVar (gw ^. gwGraph)

        let newSet   = HS.delete name (graph ^. graphParameters)
            newData  = M.delete name (graph ^. graphData)
            newGraph = graph & graphParameters .~ newSet & graphData .~ newData

        writeTVar (gw ^. gwGraph) newGraph
    redrawGraph gw



-- | Add multiple parameters to the chart. The chart then accepts parameter values 
-- for the parameters within it's '_graphParameters' field.
graphWidgetAddParameters
    :: GraphWidget
    -> [(ShortText, Ch.LineStyle, Ch.PointStyle)]
    -> IO (HashSet ShortText)
graphWidgetAddParameters gw params = do
    res <- graphAddParameters (gw ^. gwGraph) params
    redrawGraph gw
    return res

graphAddParameters
    :: TVar Graph
    -> [(ShortText, Ch.LineStyle, Ch.PointStyle)]
    -> IO (HashSet ShortText)
graphAddParameters var ls = do
    atomically $ do
        graph <- readTVar var

        let newGraph = foldl' graphAddParameter graph ls

        writeTVar var newGraph
        return (newGraph ^. graphParameters)



chart :: Graph -> Renderable ()
chart Graph {..} = toRenderable layout
  where
    properties = _graphProperties 
    plots = map (plotValToPlot . snd) $ M.toList _graphData
    layout =
        chartLayout properties _graphTimeAxisSettings
            &  layout_title
            .~ T.unpack (properties ^. gpTitle)
            &  layout_plots
            .~ plots

drawingFunction :: GraphWidget -> GI.Context -> IO Bool
drawingFunction w ctx = do
    graph <- readTVarIO (w ^. gwGraph)
    let drawingArea = w ^. gwDrawingArea
    width  <- fromIntegral <$> #getAllocatedWidth drawingArea
    height <- fromIntegral <$> #getAllocatedHeight drawingArea

    let rndr = runBackend (defaultEnv bitmapAlignmentFns)
                          (render (chart graph) (width, height))
    pickFn <- renderWithContext rndr ctx
    atomically $ writeTVar (w ^. gwPickFn) (Just pickFn)
    return True




createPopupMenu :: GraphWidget -> IO Gtk.Menu
createPopupMenu gw = do
    graph <- readTVarIO (gw ^. gwGraph)
    let names = ST.toText <$> graphGetParameterNames graph

    menu <- Gtk.menuNew
    item <- Gtk.menuItemNewWithLabel "Properties..."
    Gtk.menuShellAppend menu item
    void $ GI.on item #activate (showPropertiesDialog gw)

    item2 <- Gtk.menuItemNewWithLabel "Clear Graph..."
    Gtk.menuShellAppend menu item2
    void $ GI.on item2 #activate (graphWidgetClearValues gw)

    item3 <- Gtk.menuItemNewWithLabel "Save to SVG..."
    Gtk.menuShellAppend menu item3
    void $ GI.on item3 #activate (graphWidgetSaveToSVGAction gw)

    sep <- Gtk.separatorMenuItemNew
    Gtk.menuShellAppend menu sep

    removeMenu <- Gtk.menuNew
    removeItem <- Gtk.menuItemNewWithLabel "Remove Parameter..."
    Gtk.menuShellAppend menu removeItem
    Gtk.menuItemSetSubmenu removeItem (Just removeMenu)

    let
        menuItemAdd nm = do
            it <- Gtk.menuItemNewWithLabel nm
            Gtk.menuShellAppend removeMenu it
            void $ GI.on it
                         #activate
                         (graphWidgetRemoveParameter gw (ST.fromText nm))

    forM_ names menuItemAdd
    Gtk.widgetShowAll menu
    return menu


showPropertiesDialog :: GraphWidget -> IO ()
showPropertiesDialog g = do
    let diag = g ^. gwPropertiesDialog

    gr <- readTVarIO (g ^. gwGraph)
    setProperties diag (gr ^. graphProperties)

    resp <- Gtk.dialogRun (diag ^. gpdDialog)
    Gtk.widgetHide (diag ^. gpdDialog)
    case toEnum (fromIntegral resp) of
        Gtk.ResponseTypeOk -> do
            newProps <- getProperties diag
            applyProperties g newProps
            return ()
        _ -> return ()


applyProperties :: GraphWidget -> GraphProperties -> IO ()
applyProperties gw gp = do
    atomically $ do
        graph <- readTVar (gw ^. gwGraph)
        let !newGraph = graph & graphProperties .~ gp
        writeTVar (gw ^. gwGraph) newGraph
    return ()


setupGraphPropertiesDialog
    :: Gtk.Window -> GraphProperties -> IO GraphPropertiesDialog
setupGraphPropertiesDialog window props = do

    builder <- builderNewFromResource "/auris/data/ChartWidget.glade"

    diag    <- getObject builder "graphPropertiesDialog" Gtk.Dialog
    Gtk.windowSetTransientFor diag (Just window)

    title   <- getObject builder "graphTitleEntry" Gtk.Entry
    bgColor <- getObject builder "colorButtonBackground" Gtk.ColorButton

    void $ Gtk.dialogAddButton
        diag
        "Cancel"
        (fromIntegral (fromEnum Gtk.ResponseTypeCancel))
    void $ Gtk.dialogAddButton diag
                               "OK"
                               (fromIntegral (fromEnum Gtk.ResponseTypeOk))

    Gtk.entrySetText title (props ^. gpTitle)

    let g = GraphPropertiesDialog { _gpdDialog  = diag
                                  , _gpdTitle   = title
                                  , _gpdBGColor = bgColor
                                  }

    return g


setProperties :: GraphPropertiesDialog -> GraphProperties -> IO ()
setProperties g props = do
    Gtk.entrySetText (g ^. gpdTitle) (props ^. gpTitle)
    bg <- convertColour (props ^. gpBGColor)
    Gtk.colorChooserSetRgba (g ^. gpdBGColor) bg

getProperties :: GraphPropertiesDialog -> IO GraphProperties
getProperties g = do
    title   <- Gtk.entryGetText (g ^. gpdTitle)
    bg      <- Gtk.colorChooserGetRgba (g ^. gpdBGColor)
    bgColor <- convertRGBA bg

    let !p = GraphProperties { _gpTitle = title, _gpBGColor = bgColor }

    return p
