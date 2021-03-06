{-# LANGUAGE 
  TemplateHaskell 
  , TypeApplications
#-}
module GUI.Chart
  ( Graph(..)
  , graphGetParameterNames
  , graphInsertParamValue
  , graphClearValues 
  , graphAddParameter
  , graphRemoveParameter
  , graphName 
  , graphParameters
  , graphData 
  , graphTimeAxisSettings 
  , emptyGraph
  , PlotVal(..)
  , TimeScaleSettings(..)
  , plotValName
  , plotValLineType
  , plotValPointStyle
  , plotValValues
  , plotValToPlot
  , chartLayout
  , chartStyles
  , defaultTimeScaleSettings
  , timeScaleSettings
  )
where


import           RIO hiding ((.~))
import           RIO.List                       ( cycle )
import qualified RIO.Map                       as M
import qualified RIO.HashSet                   as HS
import qualified RIO.Text as T
import qualified Data.Text.Short               as ST


import           Data.MultiSet                  ( MultiSet )
import qualified Data.MultiSet                 as MS
import           Data.Text.Short                ( ShortText )

import qualified Data.Time.Clock               as TI
import           Data.Time.Calendar

import           Data.Colour.SRGB

import           Graphics.Rendering.Chart as Ch
import           Graphics.Rendering.Chart.Easy as Ch
                                         hiding ( (^.), (%~))

import           General.Time 

import           Data.TM.Parameter
import           Data.TM.Value



-- | The internal value of a graph
type GraphVal = (TI.UTCTime, Double)


-- | Defines a plot. For the parameter with the given 
-- name, the 'LineStyle' and 'PointStyle' are assigned, as well as the individual
-- plot values contained in a 'MultiSet', so that they are time-ordered
data PlotVal = PlotVal {
  _plotValName :: !ShortText
  , _plotValLineType :: !Ch.LineStyle
  , _plotValPointStyle :: !Ch.PointStyle
  , _plotValUsePointStyle :: !Bool
  , _plotValValues :: MultiSet GraphVal
} 
makeLenses ''PlotVal

instance Show PlotVal where 
  show x = "PlotVal {_plotValName = " <> show (x ^. plotValName) <> ", plotValValues = " 
    <> show (x ^. plotValValues) <> "}"


data TimeScaleSettings = TimeScaleSettings {
  _tssInterval :: !TI.NominalDiffTime
  , _tssOldMin :: !TI.UTCTime
  , _tssOldMax :: !TI.UTCTime
} deriving Show

timeScaleSettings :: TI.UTCTime -> TimeScaleSettings
timeScaleSettings now =
  let range = TI.secondsToNominalDiffTime 60
  in  TimeScaleSettings { _tssInterval = range
                        , _tssOldMin   = now
                        , _tssOldMax   = TI.addUTCTime range now
                        }

defaultTimeScaleSettings :: TimeScaleSettings
defaultTimeScaleSettings =
  let !range = TI.secondsToNominalDiffTime 60
      !defTime = TI.UTCTime (ModifiedJulianDay 0) (TI.secondsToDiffTime 0)
  in  TimeScaleSettings { _tssInterval = range
                        , _tssOldMin   = defTime
                        , _tssOldMax   = TI.addUTCTime range defTime
                        }



chartColors :: [AlphaColour Double]
chartColors = cycle
  [ opaque white
  , opaque yellow
  , opaque forestgreen
  , opaque firebrick
  , opaque azure
  , opaque lightgreen 
  , opaque fuchsia
  , opaque gold
  , opaque blanchedalmond
  , opaque blue
  , opaque hotpink
  ]

pointShapes :: [PointShape]
pointShapes = cycle 
 [
   PointShapeCross
   , PointShapePlus
   , PointShapeStar
   , PointShapeCircle 
   , PointShapePolygon 3 True 
   , PointShapePolygon 4 True 
   , PointShapePolygon 5 True 
   , PointShapePolygon 6 True 
 ]


chartStyles :: [(Ch.LineStyle, Ch.PointStyle)]
chartStyles = zipWith func chartColors pointShapes
  where 
    lineFunc x = def & line_color .~ x 
    pointFunc x p = def & point_color .~ x  & point_shape .~ p
    func x p = (lineFunc x, pointFunc x p)


plotValToPlot :: PlotVal -> Plot TI.UTCTime Double
plotValToPlot PlotVal {..} =
  let lin = def & plot_lines_title  .~ ST.unpack _plotValName
                & plot_lines_values .~ [values]
                & plot_lines_style  .~ _plotValLineType

      pnt = def & plot_points_style .~ _plotValPointStyle 
                & plot_points_values .~ values 
  in if _plotValUsePointStyle 
    then joinPlot (toPlot lin) (toPlot pnt)
    else toPlot lin 
  where 
    values = MS.toList _plotValValues


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
layoutTimeAxis _settings =
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


-- | Create a 'Layout' to be used with the charts
chartLayout :: TimeScaleSettings -> Layout TI.UTCTime Double
chartLayout settings =
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





data Graph = Graph {
  _graphName :: !Text
  , _graphParameters :: HashSet ShortText
  , _graphData :: Map ShortText PlotVal
  , _graphTimeAxisSettings :: TimeScaleSettings
  } deriving (Show)
makeLenses ''Graph

graphGetParameterNames :: Graph -> [ShortText]
graphGetParameterNames g = toList (g ^. graphParameters)


emptyGraph :: Text -> Graph
emptyGraph name = Graph name HS.empty M.empty defaultTimeScaleSettings

graphInsertParamValue :: Graph -> TMParameter -> Graph
graphInsertParamValue g@Graph {..} param =
  let paramName = param ^. pName
  in case M.lookup paramName _graphData of
        Nothing -> g
        Just plotVal ->
          let val = (toUTCTime (param ^. pTime), toDouble (param ^. pValue))
              newSet     = MS.insert val (plotVal ^. plotValValues)
              newPlotVal = plotVal & plotValValues .~ newSet
              newMap     = M.insert paramName newPlotVal _graphData
              newGraph   = g & graphData .~ newMap
          in newGraph


graphClearValues :: Graph -> Graph 
graphClearValues g = 
  let dat = M.toList (g ^. graphData)
      new = map (\(x, y) -> (x, y & plotValValues .~ MS.empty)) dat 
  in g & graphData .~ M.fromList new 


graphAddParameter :: Graph -> (ShortText, Ch.LineStyle, Ch.PointStyle) -> Graph
graphAddParameter graph (name, lineStyle, pointStyle) =
  let newSet   = HS.insert name (graph ^. graphParameters)
      newGraph = graph & graphParameters .~ newSet & graphData .~ newData
      -- if we already have the parameter inserted, use the old values
      newData  = M.insertWith combine
                              name
                              (PlotVal name lineStyle pointStyle False MS.empty)
                              (graph ^. graphData)
      combine _ old = old
  in newGraph

graphRemoveParameter :: Graph -> ShortText -> Graph
graphRemoveParameter graph name = 
    let newSet   = HS.delete name (graph ^. graphParameters)
        newData  = M.delete name (graph ^. graphData)
        newGraph = graph & graphParameters .~ newSet & graphData .~ newData
    in newGraph
