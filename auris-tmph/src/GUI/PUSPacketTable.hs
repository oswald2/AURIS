{-# LANGUAGE OverloadedStrings
    , BangPatterns
    , GeneralizedNewtypeDeriving
    , DeriveGeneric
    , RecordWildCards
#-}
module GUI.PUSPacketTable
    (
    setupTable

    )
where

import RIO
import qualified RIO.Text as T

import Graphics.UI.FLTK.LowLevel.FLTKHS
--import qualified Graphics.UI.FLTK.LowLevel.FL as FL
import Graphics.UI.FLTK.LowLevel.Fl_Enumerations

import GUI.Colors



setupTable :: Ref Group -> IO (Ref TableRow)
setupTable group = do
    rect <- getRectangle group
    table <- tableRowNew rect
                Nothing
                Nothing
                drawCell
                defaultCustomWidgetFuncs
                defaultCustomTableFuncs
    initializeTable table
    add group table

    pure table


maxRows :: Int
maxRows = 8000
maxCols :: Int
maxCols = 26


initializeTable :: Ref TableRow -> IO ()
initializeTable table = do
    begin table
    setColor table mcsBackground
    setColorWithBgSel table mcsBackground mcsTableBG
    setLabelcolor table mcsFontColor
    setRows table (Rows maxRows)
    setRowHeader table True
    setRowHeightAll table 20
    setRowResize table False
    setColHeaderColor table mcsWidgetBG
    setRowHeaderColor table mcsWidgetBG
    setType table SelectMulti
    setCols table (Columns maxCols)
    setColHeader table True
    setColWidthAll table 80
    setColResize table True
    end table

drawCell :: Ref TableRow -> TableContext -> TableCoordinate -> Rectangle -> IO ()
drawCell table context (TableCoordinate (Row row) (Column col)) rectangle = do
    case context of
        ContextStartPage -> flcSetFont helvetica (FontSize 14)
        ContextColHeader -> drawHeader table (T.pack (show col)) rectangle
        ContextRowHeader -> drawHeader table (T.pack (show row)) rectangle
        ContextCell -> drawData table "content" rectangle
        _ -> pure ()


drawHeader :: Ref TableRow -> Text -> Rectangle -> IO ()
drawHeader table s rectangle = do
    flcPushClip rectangle
    rhc <- getRowHeaderColor table
    flcDrawBox ThinUpBox rectangle rhc
    flcSetColor mcsTableFG
    flcDrawInBox s rectangle alignCenter Nothing Nothing
    flcPopClip

drawData :: Ref TableRow -> Text -> Rectangle -> IO ()
drawData table s rectangle = do
    flcPushClip rectangle
    flcSetColor mcsTableBG
    flcRectf rectangle
    flcSetColor mcsTableFG
    flcDrawInBox s rectangle alignLeft Nothing Nothing
    color' <- getColor table
    flcSetColor color'
    flcRect rectangle
    flcPopClip

