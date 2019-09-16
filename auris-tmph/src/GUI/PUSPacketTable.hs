{-# LANGUAGE OverloadedStrings
    , BangPatterns
    , GeneralizedNewtypeDeriving
    , DeriveGeneric
    , RecordWildCards
#-}
module GUI.PUSPacketTable
  ( setupTable
  )
where

import           RIO
import qualified RIO.Text                      as T
import qualified RIO.Vector                    as V
import qualified RIO.Vector.Partial            as V

import qualified Data.Sequence                 as S

import           Graphics.UI.FLTK.LowLevel.FLTKHS
--import qualified Graphics.UI.FLTK.LowLevel.FL as FL
import           Graphics.UI.FLTK.LowLevel.Fl_Enumerations

import           GUI.Colors

import           Model.PUSPacketModel


setupTable :: Ref Group -> TVar PUSPacketModel -> IO (Ref TableRow)
setupTable group model = do
  rect  <- getRectangle group
  table <- tableRowNew rect
                       Nothing
                       Nothing
                       (drawCell model)
                       defaultCustomWidgetFuncs
                       defaultCustomTableFuncs
  initializeTable table
  add group table

  pure table


maxRows :: Int
maxRows = 500


initializeTable :: Ref TableRow -> IO ()
initializeTable table = do
  begin table

  -- set colors
  setColor table mcsBackground
  setColorWithBgSel table mcsBackground mcsTableBG
  setLabelcolor table mcsFontColor
  setColHeaderColor table mcsWidgetBG
  setRowHeaderColor table mcsWidgetBG

  -- set properties
  setRows table (Rows maxRows)
  setRowHeader table False
  setRowHeightAll table 20
  setRowResize table False
  setType table SelectMulti

  let nCols = V.length colNames
  setCols table (Columns nCols)
  setColHeader table True

  mapM_ (\i -> setColWidth table (Column i) (colWidth V.! i)) [0 .. nCols - 1]

  --setColWidthAll table 80
  setColResize table True

  end table


colNames :: Vector Text
colNames =
  V.fromList ["Generation Time", "ERT", "APID", "T", "ST", "SSC", "Data"]

colWidth :: Vector Int
colWidth = V.fromList [200, 200, 60, 30, 30, 30, 800]


drawCell
  :: TVar PUSPacketModel
  -> Ref TableRow
  -> TableContext
  -> TableCoordinate
  -> Rectangle
  -> IO ()
drawCell model table context tc@(TableCoordinate (Row row) (Column col)) rectangle
  = do
    case context of
      ContextStartPage -> flcSetFont helvetica (FontSize 14)
      ContextColHeader -> drawHeader table (colNames V.! col) rectangle
      ContextRowHeader -> drawHeader table (T.pack (show row)) rectangle
      ContextCell      -> drawData table model tc rectangle
      _                -> pure ()


drawHeader :: Ref TableRow -> Text -> Rectangle -> IO ()
drawHeader table s rectangle = do
  flcPushClip rectangle
  rhc <- getRowHeaderColor table
  flcDrawBox ThinUpBox rectangle rhc
  flcSetColor mcsTableFG
  flcDrawInBox s rectangle alignCenter Nothing Nothing
  flcPopClip






drawData
  :: Ref TableRow
  -> TVar PUSPacketModel
  -> TableCoordinate
  -> Rectangle
  -> IO ()
drawData table model (TableCoordinate (Row row) (Column col)) rectangle = do
  flcPushClip rectangle
  flcSetColor mcsTableBG
  flcRectf rectangle
  flcSetColor mcsTableFG

  s <- atomically $ readTVar model
  let pkt = s S.!? row
      txt = toCellText pkt col
  flcDrawInBox txt rectangle alignLeft Nothing Nothing

  color' <- getColor table
  flcSetColor color'
  flcRect rectangle
  flcPopClip

