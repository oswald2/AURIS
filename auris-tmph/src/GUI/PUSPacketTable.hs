{-# LANGUAGE OverloadedStrings
    , BangPatterns
    , GeneralizedNewtypeDeriving
    , DeriveGeneric
    , RecordWildCards
#-}
module GUI.PUSPacketTable
  ( setupTable
  , addRow
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
import           Model.ScrollingTableModel

import           Data.PUS.PUSPacket
import           Data.PUS.ExtractedDU




setupTable :: Ref Group -> PUSPacketModel -> IO (Ref TableRow)
setupTable group model = do
  rect  <- getRectangle group
  table <- tableRowNew rect
                       Nothing
                       Nothing
                       (drawCell model)
                       defaultCustomWidgetFuncs
                       defaultCustomTableFuncs
  initializeTable table model
  add group table
  mcsGroupSetColor group

  pure table



initializeTable :: Ref TableRow -> PUSPacketModel -> IO ()
initializeTable table model = do
  begin table

  -- set colors
  setColor table mcsBackground
  setColorWithBgSel table mcsBackground mcsTableBG
  setLabelcolor table mcsFontColor
  setColHeaderColor table mcsWidgetBG
  setRowHeaderColor table mcsWidgetBG

  -- set properties
  nRows <- pusPacketModelSize model
  setRows table (Rows nRows)
  setRowHeader table False
  setRowHeightAll table 20
  setRowResize table False
  setType table SelectMulti

  let nCols = V.length colDefinitions
  setCols table (Columns nCols)
  setColHeader table True

  mapM_ (\i -> setColWidth table (Column i) (_columnWidth (colDefinitions V.! i))) [0 .. nCols - 1]

  --setColWidthAll table 80
  setColResize table True

  end table


addRow :: Ref TableRow -> PUSPacketModel -> ExtractedDU PUSPacket -> IO ()
addRow table model pkt = do
  (Rows nRows) <- getRows table
  when (nRows < modelMaxRows) $ setRows table (Rows (nRows + 1))
  void $ addPacketToModel pkt model



-- colNames :: Vector Text
-- colNames =
--   V.fromList ["Generation Time", "ERT", "APID", "T", "ST", "SSC", "Data"]

-- colWidth :: Vector Int
-- colWidth = V.fromList [200, 200, 60, 30, 30, 30, 800]

colDefinitions :: Vector ColumnDefinition
colDefinitions = V.fromList
  [ ColumnDefinition "Generation Time" 200
  , ColumnDefinition "ERT"             200
  , ColumnDefinition "APID"            60
  , ColumnDefinition "T"               30
  , ColumnDefinition "ST"              30
  , ColumnDefinition "SSC"             50
  , ColumnDefinition "Data"            800
  ]



drawCell
  :: PUSPacketModel
  -> Ref TableRow
  -> TableContext
  -> TableCoordinate
  -> Rectangle
  -> IO ()
drawCell model table context tc@(TableCoordinate (Row row) (Column col)) rectangle
  = do
    case context of
      ContextStartPage -> do
        flcSetFont helvetica (FontSize 14)
        takeMVar (model ^. pusPktModelLock)
      ContextEndPage -> do
        putMVar (model ^. pusPktModelLock) ()
      ContextColHeader -> drawHeader table (_columnName (colDefinitions V.! col)) rectangle
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
  :: Ref TableRow -> PUSPacketModel -> TableCoordinate -> Rectangle -> IO ()
drawData table model (TableCoordinate (Row row) (Column col)) rectangle = do
  flcPushClip rectangle
  flcSetColor mcsTableBG
  flcRectf rectangle
  flcSetColor mcsTableFG

--   s <- readIORef (model ^. pusPktModelData)
--   let pkt = s S.!? row
--       txt = toCellText pkt col
  txt <- pusPacketQueryUnlocked model (\s -> toCellText (s S.!? row) col)
  flcDrawInBox txt rectangle alignLeft Nothing Nothing

  color' <- getColor table
  flcSetColor color'
  flcRect rectangle
  flcPopClip

