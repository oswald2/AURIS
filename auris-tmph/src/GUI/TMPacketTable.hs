module GUI.TMPacketTable
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

import           Model.TMPacketModel
import           Model.ScrollingTableModel

import           Data.PUS.TMPacket




setupTable :: Ref Group -> TMPacketModel -> IO (Ref TableRow)
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


initializeTable :: Ref TableRow -> TMPacketModel -> IO ()
initializeTable table model = do
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


addRow :: Ref TableRow -> TMPacketModel -> TMPacket -> IO ()
addRow table model pkt = do
  (Rows nRows) <- getRows table
  when (nRows < modelMaxRows) $ setRows table (Rows (nRows + 1))
  void $ tableModelAddValue model modelMaxRows (ModelValue pkt)
  redraw table



-- colNames :: Vector Text
-- colNames =
--   V.fromList ["Generation Time", "ERT", "APID", "T", "ST", "SSC", "Data"]

-- colWidth :: Vector Int
-- colWidth = V.fromList [200, 200, 60, 30, 30, 30, 800]

colDefinitions :: Vector ColumnDefinition
colDefinitions = V.fromList
  [ ColumnDefinition  "SPID"            100
  , ColumnDefinition  "Mnemonic"        200
  , ColumnDefinition  "Description"     350
  , ColumnDefinition "Generation Time" 200
  , ColumnDefinition "ERT"             200
  , ColumnDefinition "APID"            60
  , ColumnDefinition "T"               30
  , ColumnDefinition "ST"              30
  , ColumnDefinition "SSC"             50
  , ColumnDefinition "VC"              30
  ]



drawCell
  :: TMPacketModel
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
        tableModelLock model
      ContextEndPage -> do
        tableModelUnlock model
      ContextColHeader ->
        drawHeader table (_columnName (colDefinitions V.! col)) rectangle
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
  :: Ref TableRow -> TMPacketModel -> TableCoordinate -> Rectangle -> IO ()
drawData table model (TableCoordinate (Row row) (Column col)) rectangle = do
  flcPushClip rectangle
  flcSetColor mcsTableBG
  flcRectf rectangle
  flcSetColor mcsTableFG

  (txt, align) <- queryTableModelUnlocked
    model
    (\s -> toCellText (s S.!? row) (Column col))

  flcDrawInBox txt rectangle align Nothing Nothing

  color' <- getColor table
  flcSetColor color'
  flcRect rectangle
  flcPopClip
