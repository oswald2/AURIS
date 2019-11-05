{-|
Module      : GUI.ScrollingTable
Description : Provides actually the FLTKHS based drawing of the table
Copyright   : (c) Michael Oswald, 2019
License     : BSD-3
Maintainer  : michael.oswald@onikudaki.net
Stability   : experimental
Portability : POSIX

This module is used for drawing the table widget. A table is associated with
a 'ScrollingTableModel', which holds the data to be displayed
-}
module GUI.ScrollingTable
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
import           Graphics.UI.FLTK.LowLevel.Fl_Enumerations

import           GUI.Colors

import           Model.ScrollingTableModel





-- | Setup a FLTKHS table for a given 'TableModel' with the given
-- 'ColumnDefinition's.
setupTable
  :: (ToCellText a)
  => Ref Group
  -> TableModel a
  -> Vector ColumnDefinition
  -> IO (Ref TableRow)
setupTable group model colDefinitions = do
  rect  <- getRectangle group
  table <- tableRowNew rect
                       Nothing
                       Nothing
                       (drawCell model colDefinitions)
                       defaultCustomWidgetFuncs
                       defaultCustomTableFuncs
  initializeTable table model colDefinitions
  add group table
  mcsGroupSetColor group

  pure table


initializeTable
  :: Ref TableRow -> TableModel a -> Vector ColumnDefinition -> IO ()
initializeTable table model colDefinitions = do
  begin table

  -- set colors
  setColor table mcsBackground
  setColorWithBgSel table mcsBackground mcsTableBG
  setLabelcolor table mcsFontColor
  setColHeaderColor table mcsWidgetBG
  setRowHeaderColor table mcsWidgetBG

  -- set properties
  nRows <- tableModelSize model
  setRows table (Rows nRows)
  setRowHeader table False
  setRowHeightAll table 20
  setRowResize table False
  setType table SelectMulti

  let nCols = V.length colDefinitions
  setCols table (Columns nCols)
  setColHeader table True

  mapM_
    (\i -> setColWidth table (Column i) (_columnWidth (colDefinitions V.! i)))
    [0 .. nCols - 1]

  --setColWidthAll table 80
  setColResize table True

  end table


modelMaxRows :: Int
modelMaxRows = 200


-- | Add a new value to the table as well as to the 'TableModel'.
-- The table is redrawn afterwards
addRow :: Ref TableRow -> TableModel a -> a -> IO ()
addRow table model pkt = do
  (Rows nRows) <- getRows table
  when (nRows < modelMaxRows) $ setRows table (Rows (nRows + 1))
  void $ tableModelAddValue model modelMaxRows pkt
  redraw table


drawCell
  :: (ToCellText a)
  => TableModel a
  -> Vector ColumnDefinition
  -> Ref TableRow
  -> TableContext
  -> TableCoordinate
  -> Rectangle
  -> IO ()
drawCell model colDefinitions table context tc@(TableCoordinate (Row row) (Column col)) rectangle
  = do
    case context of
      ContextStartPage -> do
        flcSetFont helvetica (FontSize 14)
        tableModelLock model
      ContextEndPage -> do
        tableModelUnlock model
      ContextColHeader ->
        drawHeader table (_columnName (colDefinitions V.! col)) rectangle
      ContextRowHeader -> drawHeader table (T.pack (show row)) rectangle
      ContextCell -> drawData table model tc (colDefinitions V.! col) rectangle
      _ -> pure ()


drawHeader :: Ref TableRow -> Text -> Rectangle -> IO ()
drawHeader table s rectangle = do
  flcPushClip rectangle
  rhc <- getRowHeaderColor table
  flcDrawBox ThinUpBox rectangle rhc
  flcSetColor mcsTableFG
  flcDrawInBox s rectangle alignCenter Nothing Nothing
  flcPopClip


padRectangle :: Rectangle -> Int -> Rectangle
padRectangle (Rectangle (Position (X x) (Y y)) (Size (Width w) (Height h))) pad =
  Rectangle (Position (X (x + pad)) (Y y)) (Size (Width (w - 2 * pad)) (Height h))


drawData
  :: (ToCellText a)
  => Ref TableRow
  -> TableModel a
  -> TableCoordinate
  -> ColumnDefinition
  -> Rectangle
  -> IO ()
drawData table model (TableCoordinate (Row row) (Column _col)) colDef rectangle
  = do
    flcPushClip rectangle
    flcSetColor mcsTableBG
    flcRectf rectangle
    flcSetColor mcsTableFG

    cell <- queryTableModelUnlocked model (\s -> toCellText (s S.!? row) colDef)

    flcDrawInBox (_dispcText cell)
                 (padRectangle rectangle 5)
                 (_dispcAlignment cell)
                 Nothing
                 Nothing

    color' <- getColor table
    flcSetColor color'
    flcRect rectangle
    flcPopClip
