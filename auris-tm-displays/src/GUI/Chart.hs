{-# LANGUAGE 
  TemplateHaskell 
  , TypeApplications
#-}
module GUI.Chart
    ( defaultGraphProperties
    , GraphProperties(..)
    , Graph(..)
    , graphGetParameterNames
    , graphInsertParamValue
    , graphClearValues
    , graphAddParameter
    , graphRemoveParameter
    , graphParameters
    , graphProperties
    , graphData
    , graphSetTimeRange
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
    , gpTitle 
    , gpBGColor
    ) where


--import qualified RIO.Text                      as T
import RIO
    ( map,
      ($),
      Num(negate),
      Ord((>=)),
      Show(..),
      Foldable(foldl', toList),
      Semigroup((<>)),
      Bool(..),
      Double,
      Maybe(Just, Nothing),
      (&),
      (.),
      zipWith,
      (^.),
      Map,
      Text,
      HashSet )
import qualified RIO.HashSet                   as HS
import           RIO.List                       ( cycle )
import qualified RIO.Map                       as M

import           RIO.List.Partial               ( head )

import qualified Data.Text.Short               as ST


import           Data.List                      ( groupBy )
import           Data.MultiSet                  ( MultiSet )
import qualified Data.MultiSet                 as MS
import           Data.Text.Short                ( ShortText )

import           Data.Time.Calendar
import qualified Data.Time.Clock               as TI
import           Data.Traversable

import           Data.Colour.SRGB

import           Graphics.Rendering.Chart      as Ch
import           Graphics.Rendering.Chart.Easy as Ch
                                         hiding ( (%~)
                                                , (^.)
                                                )

import           General.Time

import           Data.TM.Parameter
import           Data.TM.Value



-- | The internal value of a graph
type GraphVal = (TI.UTCTime, Double)


-- | Defines a plot. For the parameter with the given 
-- name, the 'LineStyle' and 'PointStyle' are assigned, as well as the individual
-- plot values contained in a 'MultiSet', so that they are time-ordered
data PlotVal = PlotVal
    { _plotValName          :: !ShortText
    , _plotValLineType      :: !Ch.LineStyle
    , _plotValPointStyle    :: !Ch.PointStyle
    , _plotValUsePointStyle :: !Bool
    , _plotValValues        :: !(MultiSet GraphVal)
    }
makeLenses ''PlotVal

instance Show PlotVal where
    show x =
        "PlotVal {_plotValName = "
            <> show (x ^. plotValName)
            <> ", plotValValues = "
            <> show (x ^. plotValValues)
            <> "}"

data GraphProperties = GraphProperties
    { _gpTitle   :: !Text
    , _gpBGColor :: !(Colour Double)
    }
    deriving Show
makeLenses ''GraphProperties

newtype TimeScaleSettings = TimeScaleSettings
    { _tssInterval :: TI.NominalDiffTime
    }
    deriving Show

defaultTimeScaleSettings :: TimeScaleSettings
defaultTimeScaleSettings =
    let !range = TI.secondsToNominalDiffTime 60 in TimeScaleSettings range


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
    [ PointShapeCross
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
    pointFunc x p = def & point_color .~ x & point_shape .~ p
    func x p = (lineFunc x, pointFunc x p)


plotValToPlot :: PlotVal -> Plot TI.UTCTime Double
plotValToPlot PlotVal {..} =
    let lin =
            def
                &  plot_lines_title
                .~ ST.unpack _plotValName
                &  plot_lines_values
                .~ [values]
                &  plot_lines_style
                .~ _plotValLineType

        pnt =
            def
                &  plot_points_style
                .~ _plotValPointStyle
                &  plot_points_values
                .~ values
    in  if _plotValUsePointStyle
            then joinPlot (toPlot lin) (toPlot pnt)
            else toPlot lin
    where values = MS.toList _plotValValues


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


-- | Create a 'Layout' to be used with the charts
chartLayout :: GraphProperties -> TimeScaleSettings -> Layout TI.UTCTime Double
chartLayout props settings =
    let graphBgColor = props ^. gpBGColor
    in  layout_background
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




defaultGraphProperties :: GraphProperties
defaultGraphProperties =
    GraphProperties { _gpTitle = "Graph", _gpBGColor = sRGB24 0x29 0x3d 0x5d }


data Graph = Graph
    { _graphParameters       :: !(HashSet ShortText)
    , _graphData             :: !(Map ShortText PlotVal)
    , _graphProperties       :: !GraphProperties
    , _graphTimeAxisSettings :: !TimeScaleSettings
    }
    deriving Show
makeLenses ''Graph

graphGetParameterNames :: Graph -> [ShortText]
graphGetParameterNames g = toList (g ^. graphParameters)


emptyGraph :: Text -> Graph
emptyGraph name = Graph HS.empty M.empty defaultGraphProperties {_gpTitle = name } defaultTimeScaleSettings

graphSetTimeRange :: Graph -> TI.NominalDiffTime -> Graph
graphSetTimeRange g range =
    g & graphTimeAxisSettings .~ (TimeScaleSettings range)

graphInsertParamValue :: TI.UTCTime -> Graph -> [TMParameter] -> Graph
graphInsertParamValue now graph@Graph {..} params =
    let groups       = groupBy eqByName params
        timeSettings = _graphTimeAxisSettings
        lowerTime    = TI.addUTCTime (negate (_tssInterval timeSettings)) now
    in  foldl' (ins lowerTime) graph groups
  where
    ins lowerTime g@Graph {..} paramGroup =
        let paramName = (head paramGroup) ^. pName
        in
            case M.lookup paramName _graphData of
                Nothing -> g
                Just plotVal ->
                    let
                        -- insert new values into multiset (time ordered)
                        newSet = foldl' insertParam
                                        (plotVal ^. plotValValues)
                                        paramGroup
                        -- remove values from the set, that are out of the time range to display
                        validSet = MS.filter (\(t, _) -> t >= lowerTime) newSet
                        -- now set the values in the plotVal again 
                        newPlotVal = plotVal & plotValValues .~ validSet
                        -- also update the plotValue in the map (parameter name -> PlotValue)
                        newMap     = M.insert paramName newPlotVal _graphData
                        -- and update the graph itself 
                        newGraph   = g & graphData .~ newMap
                    in
                        newGraph

    insertParam mSet param =
        let val = (toUTCTime (param ^. pTime), toDouble (param ^. pValue))
        in  MS.insert val mSet


graphClearValues :: Graph -> Graph
graphClearValues g =
    let dat = M.toList (g ^. graphData)
        new = map (\(x, y) -> (x, y & plotValValues .~ MS.empty)) dat
    in  g & graphData .~ M.fromList new


graphAddParameter :: Graph -> (ShortText, Ch.LineStyle, Ch.PointStyle) -> Graph
graphAddParameter graph (name, lineStyle, pointStyle) =
    let newSet   = HS.insert name (graph ^. graphParameters)
        newGraph = graph & graphParameters .~ newSet & graphData .~ newData
        -- if we already have the parameter inserted, use the old values
        newData  = M.insertWith
            combine
            name
            (PlotVal name lineStyle pointStyle False MS.empty)
            (graph ^. graphData)
        combine _ old = old
    in  newGraph

graphRemoveParameter :: Graph -> ShortText -> Graph
graphRemoveParameter graph name =
    let newSet   = HS.delete name (graph ^. graphParameters)
        newData  = M.delete name (graph ^. graphData)
        newGraph = graph & graphParameters .~ newSet & graphData .~ newData
    in  newGraph
