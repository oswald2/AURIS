{-# LANGUAGE TemplateHaskell #-}
module GUI.StatisticsTab
    ( StatisticsTab
    , createStatisticsTab
    , statisticsTabDisplayStats
    , setupCallbacks
    ) where

import           RIO

import           Control.Lens                   ( makeLenses )

import           Text.Builder

import           Data.Sequence                 as S

import           Data.PUS.Statistics

import qualified GI.Cairo                      as GI
import           GI.Gtk                        as Gtk
import           Graphics.Rendering.Chart      as Ch
import           Graphics.Rendering.Chart.Backend.GI.Cairo
import           Graphics.Rendering.Chart.Easy as Ch
                                         hiding ( (.~)
                                                , (^.)
                                                )

import           GI.Cairo.Render.Connector      ( renderWithContext )

import           GUI.Utils

import           Data.Time.Clock

import           Interface.Interface



type ChartData = [DataPoint]
type DataPoint = (UTCTime, Double)


data StatisticsTab = StatisticsTab
    { _statTmFrameRate          :: !Entry
    , _statTmFrameDataRate      :: !Entry
    , _statTmFrameTotalRate     :: !Entry
    , _statTmFrameTotalDataRate :: !Entry
    , _statTmFrameN             :: !Entry
    , _statTmFrameReset         :: !Button
    , _statTmFrameCopy          :: !Button
    , _statTmFrameChartArea     :: !DrawingArea
    , _statTmFrameData          :: TVar (Seq DataPoint, Seq DataPoint)
    , _statTmPktRate            :: !Entry
    , _statTmPktDataRate        :: !Entry
    , _statTmPktTotalRate       :: !Entry
    , _statTmPktTotalDataRate   :: !Entry
    , _statTmPktN               :: !Entry
    , _statTmPktReset           :: !Button
    , _statTmPktCopy            :: !Button
    , _statTmPktChartArea       :: !DrawingArea
    , _statTmPktData
          :: TVar (Seq DataPoint, Seq DataPoint, Seq DataPoint, Seq DataPoint)
    }
makeLenses ''StatisticsTab



createStatisticsTab :: Gtk.Builder -> IO StatisticsTab
createStatisticsTab builder = do
    frameRate          <- getObject builder "labelStatFrameRate" Entry
    frameDataRate      <- getObject builder "labelStatFrameDataRate" Entry
    frameTotalRate     <- getObject builder "labelStatFrameTotalRate" Entry
    frameTotalDataRate <- getObject builder "labelStatFrameTotalDataRate" Entry
    frameN             <- getObject builder "labelStatFramesN" Entry
    frameReset         <- getObject builder "buttonStatFrameReset" Button
    frameCopy          <- getObject builder "buttonStatFrameCopy" Button
    frameChartArea     <- getObject builder "drawingAreaFrames" DrawingArea
    pktRate            <- getObject builder "labelStatPktRate" Entry
    pktDataRate        <- getObject builder "labelStatPktDataRate" Entry
    pktTotalRate       <- getObject builder "labelStatPktTotalRate" Entry
    pktTotalDataRate   <- getObject builder "labelStatPktTotalDataRate" Entry
    pktN               <- getObject builder "entryStatPktsN" Entry
    pktReset           <- getObject builder "buttonStatPktReset" Button
    pktCopy            <- getObject builder "buttonStatPktCopy" Button
    pktChartArea       <- getObject builder "drawingAreaPackets" DrawingArea

    frameData          <- newTVarIO (S.empty, S.empty)
    pktData            <- newTVarIO (S.empty, S.empty, S.empty, S.empty)

    let gui = StatisticsTab { _statTmFrameRate          = frameRate
                            , _statTmFrameDataRate      = frameDataRate
                            , _statTmFrameTotalRate     = frameTotalRate
                            , _statTmFrameTotalDataRate = frameTotalDataRate
                            , _statTmFrameN             = frameN
                            , _statTmFrameReset         = frameReset
                            , _statTmFrameCopy          = frameCopy
                            , _statTmFrameChartArea     = frameChartArea
                            , _statTmFrameData          = frameData
                            , _statTmPktRate            = pktRate
                            , _statTmPktDataRate        = pktDataRate
                            , _statTmPktTotalRate       = pktTotalRate
                            , _statTmPktTotalDataRate   = pktTotalDataRate
                            , _statTmPktN               = pktN
                            , _statTmPktReset           = pktReset
                            , _statTmPktCopy            = pktCopy
                            , _statTmPktChartArea       = pktChartArea
                            , _statTmPktData            = pktData
                            }

    void $ Gtk.on frameChartArea #draw (drawingFunctionFrame gui)
    void $ Gtk.on pktChartArea #draw (drawingFunctionPacket gui)

    pure gui


setupCallbacks :: StatisticsTab -> Interface -> IO ()
setupCallbacks gui interface = do
    void $ Gtk.on (gui ^. statTmFrameReset) #clicked $ do
        callInterface interface actionStatResetFrames

    void $ Gtk.on (gui ^. statTmPktReset) #clicked $ do
        callInterface interface actionStatResetPackets


maxData :: Int
maxData = 600

addStatisticsData :: StatisticsTab -> TMStatistics -> IO ()
addStatisticsData gui statistics = do
    let stats  = _statFrame statistics
        pstats = _statPackets statistics
    when (tmStatFrameTime stats /= nullTimeStamp) $ do
        atomically $ do
            (rate, totalRate) <- readTVar (gui ^. statTmFrameData)
            let t               = toUTCTime (tmStatFrameTime stats)
                newRateVal      = (t, tmStatFrameRate stats)
                newTotalRateVal = (t, tmStatFrameTotal stats)
                !newRate        = insSeq rate newRateVal
                !newTotalRate   = insSeq totalRate newTotalRateVal
            writeTVar (gui ^. statTmFrameData) (newRate, newTotalRate)

            (prate, ptotalRate, pdata, pTotalData) <- readTVar
                (gui ^. statTmPktData)
            let t1               = toUTCTime (tmStatPacketTime pstats)
                newPRateVal      = (t1, tmStatPacketRate pstats)
                newPTotalRateVal = (t1, tmStatPacketTotal pstats)
                newPDataVal      = (t1, tmStatPacketBytes pstats)
                newPTotalDataVal = (t1, tmStatPacketTotalBytes pstats)
                !newPRate        = insSeq prate newPRateVal
                !newPTotalRate   = insSeq ptotalRate newPTotalRateVal
                !newPData        = insSeq pdata newPDataVal
                !newPTotalData   = insSeq pTotalData newPTotalDataVal
            writeTVar (gui ^. statTmPktData)
                      (newPRate, newPTotalRate, newPData, newPTotalData)

insSeq :: Seq a -> a -> Seq a
insSeq s d = if S.length s > maxData then (S.drop 1 s) S.|> d else s S.|> d





getFrameData :: StatisticsTab -> IO (ChartData, ChartData)
getFrameData gui = do
    (fr, ftr) <- readTVarIO (gui ^. statTmFrameData)
    return (toList fr, toList ftr)

getPacketData
    :: StatisticsTab -> IO (ChartData, ChartData, ChartData, ChartData)
getPacketData gui = do
    (pr, ptr, pdr, ptdr) <- readTVarIO (gui ^. statTmPktData)
    return (toList pr, toList ptr, toList pdr, toList ptdr)


statisticsTabDisplayStats :: StatisticsTab -> TMStatistics -> IO ()
statisticsTabDisplayStats gui stats = do
    let fr = _statFrame stats
        pr = _statPackets stats

    addStatisticsData gui stats

    entrySetText (gui ^. statTmFrameRate) (formatRate (tmStatFrameRate fr))
    entrySetText (gui ^. statTmFrameTotalRate)
                 (formatRate (tmStatFrameTotal fr))
    entrySetText (gui ^. statTmFrameDataRate)
                 (formatDataRate (tmStatFrameBytes fr))
    entrySetText (gui ^. statTmFrameTotalDataRate)
                 (formatDataRate (tmStatFrameTotalBytes fr))
    entrySetText (gui ^. statTmFrameN)       (run (decimal (tmStatFramesN fr)))

    entrySetText (gui ^. statTmPktRate)      (formatRate (tmStatPacketRate pr))
    entrySetText (gui ^. statTmPktTotalRate) (formatRate (tmStatPacketTotal pr))
    entrySetText (gui ^. statTmPktDataRate)
                 (formatDataRate (tmStatPacketBytes pr))
    entrySetText (gui ^. statTmPktTotalDataRate)
                 (formatDataRate (tmStatPacketTotalBytes pr))
    entrySetText (gui ^. statTmPktN) (run (decimal (tmStatPacketsN pr)))

    redrawGraphs gui
  where
    formatRate x = run $ fixedDouble 3 x
    formatDataRate n
        | n > 1_000_000_000 = run $ fixedDouble 3 (n / 1_000_000_000) <> " GB/s"
        | n > 1_000_000     = run $ fixedDouble 3 (n / 1_000_000) <> " MB/s"
        | n > 1_000         = run $ fixedDouble 3 (n / 1_000) <> " KB/s"
        | otherwise         = run $ fixedDouble 3 n <> " B/s"


redrawGraphs :: StatisticsTab -> IO ()
redrawGraphs gui = do
    Gtk.widgetQueueDraw (gui ^. statTmFrameChartArea)
    Gtk.widgetQueueDraw (gui ^. statTmPktChartArea)


drawingFunctionFrame :: StatisticsTab -> GI.Context -> IO Bool
drawingFunctionFrame gui context = do
    frameData <- getFrameData gui
    let drawingArea = gui ^. statTmFrameChartArea
    width  <- fromIntegral <$> #getAllocatedWidth drawingArea
    height <- fromIntegral <$> #getAllocatedHeight drawingArea

    let rndr = runBackend (defaultEnv bitmapAlignmentFns)
                          (render (frameChart frameData) (width, height))
    void $ renderWithContext rndr context
    --atomically $ writeTVar (w ^. gwPickFn) (Just pickFn)
    return True

drawingFunctionPacket :: StatisticsTab -> GI.Context -> IO Bool
drawingFunctionPacket gui context = do
    pktData <- getPacketData gui
    let drawingArea = gui ^. statTmPktChartArea
    width  <- fromIntegral <$> #getAllocatedWidth drawingArea
    height <- fromIntegral <$> #getAllocatedHeight drawingArea

    let rndr = runBackend (defaultEnv bitmapAlignmentFns)
                          (render (packetChart pktData) (width, height))
    void $ renderWithContext rndr context
    --atomically $ writeTVar (w ^. gwPickFn) (Just pickFn)
    return True


initialFrameChart :: (PlotLines x1 y1, PlotLines x2 y2)
initialFrameChart = (rate, totalRate)
  where
    rate =
        plot_lines_style
            .  line_color
            .~ opaque orange
            $  plot_lines_style
            .  line_width
            .~ 2
            $  plot_lines_title
            .~ "Frame Rate (frames/sec)"
            $  def

    totalRate =
        plot_lines_style
            .  line_color
            .~ opaque red
            $  plot_lines_style
            .  line_width
            .~ 2
            $  plot_lines_title
            .~ "Total Frame Rate (frames/sec)"
            $  def


frameChart :: (ChartData, ChartData) -> Renderable ()
frameChart (frameRate, frameTotalRate) = toRenderable layout
  where
    (rate, totalRate) = initialFrameChart
    rate'             = plot_lines_values .~ [frameRate] $ rate

    totalRate'        = plot_lines_values .~ [frameTotalRate] $ totalRate

    layout =
        layout_title
            .~ "Frame Rate"
            $  layout_plots
            .~ [toPlot rate', toPlot totalRate']
            $  def



initialPacketChart
    :: (PlotLines x1 y1, PlotLines x2 y2, PlotLines x3 y3, PlotLines x4 y4)
initialPacketChart = (rate, totalRate, dataRate, totalDataRate)
  where
    rate =
        plot_lines_style
            .  line_color
            .~ opaque limegreen
            $  plot_lines_style
            .  line_width
            .~ 2
            $  plot_lines_title
            .~ "Packet Rate (pkts/sec)"
            $  def

    totalRate =
        plot_lines_style
            .  line_color
            .~ opaque darkviolet
            $  plot_lines_style
            .  line_width
            .~ 2
            $  plot_lines_title
            .~ "Total Packet Rate (pkts/sec)"
            $  def

    dataRate =
        plot_lines_style
            .  line_color
            .~ opaque darkgreen
            $  plot_lines_style
            .  line_width
            .~ 2
            $  plot_lines_title
            .~ "Data Rate (Bytes/sec)"
            $  def

    totalDataRate =
        plot_lines_style
            .  line_color
            .~ opaque darkblue
            $  plot_lines_style
            .  line_width
            .~ 2
            $  plot_lines_title
            .~ "Total Data Rate (Bytes/sec)"
            $  def


packetChart :: (ChartData, ChartData, ChartData, ChartData) -> Renderable ()
packetChart (packetRate, packetTotalRate, packetDataRate, packetTotalDataRate)
    = toRenderable layout
  where
    (rate, totalRate, dataRate, totalDataRate) = initialPacketChart
    rate'          = plot_lines_values .~ [packetRate] $ rate
    totalRate'     = plot_lines_values .~ [packetTotalRate] $ totalRate
    dataRate'      = plot_lines_values .~ [packetDataRate] $ dataRate
    totalDataRate' = plot_lines_values .~ [packetTotalDataRate] $ totalDataRate

    layout =
        layoutlr_title
            .~ "Packet Rate"
            $  layoutlr_plots
            .~ [ Left (toPlot rate')
               , Left (toPlot totalRate')
               , Right (toPlot dataRate')
               , Right (toPlot totalDataRate')
               ]
            $  def

