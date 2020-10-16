{-# LANGUAGE 
  TemplateHaskell
#-}
module GUI.NameDescrTable
  ( TableValue(..)
  , NameDescrTable
  , createNameDescrTable
  , getSelectedItems
  , getSelectedItemsVector
  , setTableFromModel
  , setPopupMenu
  )
where

import           RIO
import qualified RIO.Text as T
import qualified RIO.Vector as V
import           Control.Lens                   ( makeLenses
                                                )
import           GI.Gtk                        as Gtk
import           Data.GI.Gtk.ModelView.SeqStore
import           Data.GI.Gtk.ModelView.CellLayout
import           GI.Gdk.Structs.EventButton



data TableValue = TableValue {
  _tableValName :: !Text
  , _tableValDescr :: !Text
  } deriving (Show)
makeLenses ''TableValue

data NameDescrTable = NameDescrTable {
  _nmdtView :: !TreeView 
  , _nmdtModel :: !(SeqStore TableValue)
  , _nmdtFilterModel :: !TreeModelFilter
  , _nmdtMenu :: TVar (Maybe Menu)
  }
makeLenses ''NameDescrTable


-- | Create a new 'NameDescrTable' within the given 'Box' and the 
-- given list of 'TableValue'. The table is a 'TreeView' with a 
-- filter model. At the bottom, a 'Entry' is added to enter the 
-- filter values. 
createNameDescrTable :: Box -> [TableValue] -> IO NameDescrTable
createNameDescrTable mainBox values = do 
  model <- seqStoreNew values 

  --treeViewSetModel tv (Just model)

  -- add a filter entry and a label 
  filterEntry <- new Entry []
  filterLabel <- new Label [ #label := "Filter:"]
  box <- boxNew OrientationHorizontal 0 
  boxPackStart box filterLabel False False 5 
  boxPackStart box filterEntry True True 5 

  -- the main box is the treeview in a scrolled window and the 
  -- filter entry box (label + entry) below
  scrolledWin <- new ScrolledWindow []
  tv <- new TreeView [ #enableGridLines := TreeViewGridLinesBoth, 
    #headersVisible := True, 
    #rulesHint := True, 
    #searchColumn := 0 ]
  containerAdd scrolledWin tv 

  boxPackStart mainBox scrolledWin True True 5 
  boxPackStart mainBox box False False 5 

  -- create a filter model and set the child model
  filterModel <- new TreeModelFilter [ #childModel := model ] 
  -- set the filter function (see below)
  treeModelFilterSetVisibleFunc filterModel (filterFunction model filterEntry)
  -- now set the filter model for the TreeView
  treeViewSetModel tv (Just filterModel)

  -- set callback to retrigger the filtering 
  void $ after filterEntry #keyReleaseEvent (const $ treeModelFilterRefilter filterModel >> return False)

  selection <- treeViewGetSelection tv 

  treeSelectionSetMode selection SelectionModeMultiple
  treeViewSetRubberBanding tv True

  col1 <- treeViewColumnNew
  col2 <- treeViewColumnNew

  treeViewColumnSetTitle col1 "Name"
  treeViewColumnSetTitle col2 "Description"

  renderer1 <- cellRendererTextNew 
  renderer2 <- cellRendererTextNew 

  cellLayoutPackStart col1 renderer1 True 
  cellLayoutPackStart col2 renderer2 True 

  cellLayoutSetAttributes col1 renderer1 model $ \val -> [ #text := val ^. tableValName]
  cellLayoutSetAttributes col2 renderer2 model $ \val -> [ #text := val ^. tableValDescr]

  void $ treeViewAppendColumn tv col1
  void $ treeViewAppendColumn tv col2

  treeViewSetSearchColumn tv 0
  treeViewSetEnableSearch tv True 
  treeViewSetSearchEqualFunc tv (searchFunc model)

  menu <- newTVarIO Nothing 

  let g = NameDescrTable tv model filterModel menu 

  void $ Gtk.on tv #buttonPressEvent (buttonCB g)

  return g

  where 
    searchFunc model _ _ text iter = do 
      idx <- seqStoreIterToIndex iter 
      val <- seqStoreGetValue model idx 
      
      let searchText = T.toLower text
          !res = (searchText `T.isPrefixOf` T.toLower (val ^. tableValName)) 
            || (searchText `T.isPrefixOf` T.toLower (val ^. tableValDescr)) 
      return res 

    filterFunction model entry _ iter = do 
      idx <- seqStoreIterToIndex iter 
      val <- seqStoreGetValue model idx 
      text <- get entry #text 
      let searchText = T.toLower text
          !res = (searchText `T.isInfixOf` T.toLower (val ^. tableValName)) 
            || (searchText `T.isInfixOf` T.toLower (val ^. tableValDescr)) 
      return res 




setPopupMenu :: NameDescrTable -> Menu -> IO () 
setPopupMenu tbl menu = atomically $ writeTVar (tbl ^. nmdtMenu) (Just menu)


buttonCB :: NameDescrTable -> EventButton -> IO Bool 
buttonCB tbl evtBtn = do 
  bt <- getEventButtonButton evtBtn 
  case bt of 
    3 -> do --right mouse button 
      mn <- readTVarIO (tbl ^. nmdtMenu)
      case mn of 
        Nothing -> return False 
        Just menu -> do 
          menuPopupAtPointer menu Nothing
          return True 
    _ -> return False 


-- | gets the currently selected items in the table and returns a list of 
-- the selected values
getSelectedItems :: NameDescrTable -> IO [TableValue]
getSelectedItems tbl = do
  let tv = tbl ^. nmdtView
      model = tbl ^. nmdtModel

  sel <- treeViewGetSelection tv 
  (paths, _) <- treeSelectionGetSelectedRows sel 

  idxs <- traverse treePathGetIndices paths 
  traverse (seqStoreGetValue model) ((concat . catMaybes) idxs)

-- | gets the currently selected items in the table and returns a 'Vector' of 
-- the selected values
getSelectedItemsVector :: NameDescrTable -> IO (Vector TableValue)
getSelectedItemsVector tbl = V.fromList <$> getSelectedItems tbl



-- | refresh a table from a the given values. There is no maxRow check, so
-- the model is displayed as-is
setTableFromModel :: NameDescrTable -> Vector TableValue -> IO ()
setTableFromModel table vals = do
  let model = table ^. nmdtModel

  seqStoreClear model 
  mapM_ (seqStoreAppend model) vals



