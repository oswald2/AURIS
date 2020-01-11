module GUI.NameDescrTable
  ( TableValue(..)
  , NameDescrTable
  , TableNameDescrModel
  , setupTable
  , setTableFromModel
  , setupCallback
  , getSelectedItems
  )
where

import           RIO


import qualified RIO.Text                      as T
import qualified RIO.Vector                    as V
import qualified RIO.Vector.Partial            as V
                                                ( (!) )
import qualified Data.Text.IO                  as T
import           Graphics.UI.FLTK.LowLevel.FLTKHS
import           Graphics.UI.FLTK.LowLevel.Fl_Enumerations
import qualified Graphics.UI.FLTK.LowLevel.FL  as FL


import           GUI.Colors



data TableValue = TableValue {
  _tableValName :: !Text
  , _tableValDescr :: !Text
  } deriving (Show)



type TableNameDescrModel = TVar (Vector TableValue)


data NameDescrTable = NameDescrTable {
  _nmDescTbl :: Ref TableRow
  , _nmDescTblModel :: TableNameDescrModel
  , _nmDecTclInput :: Ref Input
}

-- | gets the currently selected items in the table and returns a list of 
-- the selected values
getSelectedItems :: NameDescrTable -> IO [TableValue]
getSelectedItems tbl = do
  let table = _nmDescTbl tbl
      filt x = do 
              either  
                (const True)
                id 
                <$> getRowSelected table (Row x)

  join $ atomically $ do 
    vec <- readTVar (_nmDescTblModel tbl)
    return $ do 
      Rows rs <- getRows table
      map (vec V.!) <$> filterM filt [0 .. (rs - 1)]


-- | Setup a FLTKHS table for a given 'TableModel' with the given
-- 'ColumnDefinition's.
setupTable :: Ref Group -> IO NameDescrTable
setupTable group = do

  model <- newTVarIO V.empty

  Rectangle (Position (X x) (Y y)) (Size (Width w) (Height h))  <- getRectangle group

  let tableRect = Rectangle (Position (X x) (Y y)) (Size (Width w) (Height (h - 20)))
      inputRect = Rectangle (Position (X x) (Y (y + h - 40))) (Size (Width w) (Height 20))

  table <- tableRowNew tableRect
                       Nothing
                       Nothing
                       (drawCell model)
                       defaultCustomWidgetFuncs
                       defaultCustomTableFuncs

  input <- inputNew inputRect Nothing (Just FlNormalInput)
  mcsInputSetColor input

  let table' = NameDescrTable table model input

  initializeTable table'
  mcsGroupSetColor group

  add group table 
  add group input 

  setCallback input (handleFilter table')

  pure table'



initializeTable :: NameDescrTable -> IO ()
initializeTable table' = do
  let table = _nmDescTbl table'
  --begin table

  -- set colors
  setColor table mcsBackground
  --setColorWithBgSel table mcsBackground mcsTableBG
  setSelectionColor table mcsTableSelectionColor
  setLabelcolor table mcsFontColor
  setColHeaderColor table mcsWidgetBG
  setRowHeaderColor table mcsWidgetBG

  -- set properties
  nRows <- V.length <$> readTVarIO (_nmDescTblModel table')
  setRows table (Rows nRows)
  setRowHeader table False
  setRowHeightAll table 20
  setRowResize table False
  setType table SelectMulti
  setWhen table [WhenRelease]

  setCols table (Columns 2)
  setColHeader table True

  setColWidth table (Column 0) 100
  setColWidth table (Column 1) 200
  setColResize table True

  --end table


-- TODO
handleFilter :: NameDescrTable -> Ref Input -> IO () 
handleFilter table _ = do
  return ()


-- | refresh a table from a model. There is no maxRow check, so
-- the model is displayed as-is
setTableFromModel :: NameDescrTable -> Vector TableValue -> IO ()
setTableFromModel table model = do
  let nRows    = V.length model

  let tableRef = _nmDescTbl table
  atomically $ writeTVar (_nmDescTblModel table) model
  setRows tableRef (Rows nRows)
  redraw tableRef



drawCell
  :: TableNameDescrModel
  -> Ref TableRow
  -> TableContext
  -> TableCoordinate
  -> Rectangle
  -> IO ()
drawCell model table context tc@(TableCoordinate (Row row) (Column col)) rectangle
  = do
    values <- readTVarIO model
    case context of
      ContextStartPage -> do
        flcSetFont helvetica (FontSize 14)
      ContextColHeader -> drawHeader
        table
        (case col of
          0 -> "Name"
          1 -> "Description"
          _ -> ""
        )
        rectangle
      ContextRowHeader -> drawHeader table (T.pack (show row)) rectangle
      ContextCell      -> case col of
        0 -> maybe (return ())
                   (\x -> drawData table (_tableValName x) tc rectangle)
                   (values V.!? row)
        1 -> maybe (return ())
                   (\x -> drawData table (_tableValDescr x) tc rectangle)
                   (values V.!? row)
        _ -> return ()
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
padRectangle (Rectangle (Position (X x) (Y y)) (Size (Width w) (Height h))) pad
  = Rectangle (Position (X (x + pad)) (Y y))
              (Size (Width (w - 2 * pad)) (Height h))


drawData :: Ref TableRow -> Text -> TableCoordinate -> Rectangle -> IO ()
drawData table text (TableCoordinate (Row row) (Column _col)) rectangle = do
  flcPushClip rectangle

  (bgColor, fgColor) <- do
    isSelected' <- getRowSelected table (Row row)
    case isSelected' of
      Right is' -> if is'
        then (, mcsTableFG) <$> getSelectionColor table
        else return (mcsTableBG, mcsTableFG)
      Left _ -> return (mcsTableBG, mcsTableFG)
  flcSetColor bgColor

  flcRectf rectangle
  flcSetColor fgColor


  flcDrawInBox text (padRectangle rectangle 5) alignLeft Nothing Nothing

  color' <- getColor table
  flcSetColor color'
  flcRect rectangle
  flcPopClip



setupCallback :: Ref TableRow -> (Row -> IO ()) -> IO ()
setupCallback table doubleClickCB = do
  setCallback table (eventCallback doubleClickCB)


eventCallback :: (Row -> IO ()) -> Ref TableRow -> IO ()
eventCallback doubleClickCB table = do
  r        <- callbackRow table
  context' <- callbackContext table
  case context' of
    ContextCell -> do
      event'       <- FL.eventIsClick
      mouseButton' <- FL.eventButton
      clicks'      <- FL.eventClicks
      case mouseButton' of
        Nothing -> return ()
        Just mb' ->
          when (event' && mb' == Mouse_Left && clicks' == 1) $ doubleClickCB r
    _ -> return ()
