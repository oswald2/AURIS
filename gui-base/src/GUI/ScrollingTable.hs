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
    ( addRowSeqStore
    , addRowScrollingTable
    , addRowSeqStoreAppend
    , setRowsSeqStore
    , addRowsSeqStoreAppend
    , setTreeViewCallback
    , createScrollingTable
    , createScrollingTableSimple
    , createScrollingTableFilter
    , createSortedScrollingTable
    , createSortedScrollingTableSimple
    , SortFunc
    , sortFuncGetValues
    ) where

import           RIO
import qualified RIO.Text                      as T
import           GI.Gtk                        as Gtk
import           Data.GI.Gtk.ModelView.SeqStore
import           Data.GI.Gtk.ModelView.CellLayout
import           Data.GI.Base.Attributes

import           GUI.Definitions
import           GUI.TreeView


-- | Generic GTK function for adding a new row in a 'SeqStore a'. This is 
-- intended for the live-view as only 'defMaxRowTM' rows will be added. When
-- this limit is reached, the oldest row will be removed first.
addRowSeqStore :: SeqStore a -> a -> IO ()
addRowSeqStore model val = do
    n <- seqStoreGetSize model
    when (n > defMaxRowTM) $ do
        seqStoreRemove model (n - 1)
    seqStorePrepend model val


-- | Generic GTK function for adding a new row in a 'SeqStore a'. This is 
-- intended for the live-view as only 'defMaxRowTM' rows will be added. When
-- this limit is reached, the oldest row will be removed first.
-- Additionally, this functions scrolls to the top, when a row was added 
addRowScrollingTable :: TreeView -> SeqStore a -> a -> IO ()
addRowScrollingTable tv model val = do
    addRowSeqStore model val
    scrollToTop tv


addRowSeqStoreAppend :: SeqStore a -> a -> IO ()
addRowSeqStoreAppend model val = do
    n <- seqStoreGetSize model
    when (n > defMaxRowTM) $ do
        seqStoreRemove model (n - 1)
    void $ seqStoreAppend model val


-- | Set the model to the given list of values, ignoring maximum size. This is 
-- intended for retrieval mode.
setRowsSeqStore :: Traversable t => SeqStore a -> t a -> IO ()
setRowsSeqStore model values = do
    seqStoreClear model
    traverse_ (seqStorePrepend model) values

-- | Add multiple rows to the model, ignorsing maximum size. This is intended
-- for e.g. batched retrievals
addRowsSeqStoreAppend :: Traversable t => SeqStore a -> t a -> IO () 
addRowsSeqStoreAppend model values = do 
    traverse_ (seqStoreAppend model) values

-- | Setup a callback for the double-click on a table. 
-- @setTreeViewCallback gui getTv getModel callback@: getTv and getModel are 
-- functions to extract a 'TreeView' and a 'SeqStore' from the @gui@ element.
-- @action@ is the callback to be called with the row which was double-clicked.
setTreeViewCallback
    :: a -> (a -> TreeView) -> (a -> SeqStore b) -> (b -> IO ()) -> IO ()
setTreeViewCallback g getTV getModel action = do
    void $ Gtk.on (getTV g) #rowActivated $ \path _col -> do
        ipath <- treePathGetIndices path
        forM_ ipath $ \idxs -> do
            case idxs of
                (idx : _) -> do
                    val <- seqStoreGetValue (getModel g) idx
                    action val
                [] -> return ()


-- | Create a scrolling table with only text in the columns. Takes the 'TreeView',
-- a constructor to return the 'TreeView' and the corresponding created 'SeqStore' 
-- for type @a@ and a list of column information. 
--
-- The column information is a tuple of the name of the column (what should be 
-- displayed in the header row) and a function to convert from a an element of the
-- model to a list of GTK attributes of the cell for the value to display 
-- for element in this column.
createScrollingTable
    :: TreeView
    -> (TreeView -> SeqStore a -> b)
    -> [(Text, Int32, a -> [AttrOp CellRendererText 'AttrSet])]
    -> IO b
createScrollingTable tv constr attribs = do
    model <- createScrollingTableSimple tv attribs
    return $ constr tv model


createScrollingTableSimple
    :: TreeView
    -> [(Text, Int32, row -> [AttrOp CellRendererText 'AttrSet])]
    -> IO (SeqStore row)
createScrollingTableSimple tv attribs = do
    model <- seqStoreNew []

    treeViewSetModel tv (Just model)

    treeViewSetHeadersVisible tv True

    mapM_ (createColumn model) attribs

    -- try to set fixed height mode for more speed
    treeViewSetFixedHeightMode tv True

    return model

  where
    createColumn model (name, width, attr) = do
        col <- treeViewColumnNew
        treeViewColumnSetFixedWidth col width
        treeViewColumnSetSizing col TreeViewColumnSizingFixed
        treeViewColumnSetResizable col True
        treeViewColumnSetReorderable col True
        treeViewColumnSetTitle col name
        renderer <- cellRendererTextNew
        cellLayoutPackStart col renderer True
        cellLayoutSetAttributes col renderer model attr
        void $ treeViewAppendColumn tv col
        return (name, col, renderer)





createScrollingTableFilter
    :: TreeView
    -> SeqStore row
    -> SearchEntry
    -> Selection 
    -> (Text -> row -> Bool)
    -> [(Text, Int32, row -> [AttrOp CellRendererText 'AttrSet])]
    -> IO (SeqStore row, TreeModelFilter)
createScrollingTableFilter tv model searchEntry sel filterFunc attribs = do
    filterModel <- new TreeModelFilter [#childModel := model]

    Gtk.set
        tv
        [ #headersVisible := True
        , #rulesHint := True
        , #searchColumn := 0
        ]
    mapM_ (createColumn model) attribs

    treeViewSetModel tv (Just filterModel)

    -- try to set fixed height mode for more speed
    treeViewSetFixedHeightMode tv True

    treeModelFilterSetVisibleFunc filterModel (filterFunction model searchEntry filterFunc)
    void $ Gtk.on searchEntry #searchChanged (treeModelFilterRefilter filterModel)

    selection <- treeViewGetSelection tv
    case sel of
        MultiSelection -> do
            treeSelectionSetMode selection SelectionModeMultiple
            treeViewSetRubberBanding tv True
        SingleSelection -> do
            treeSelectionSetMode selection SelectionModeSingle
            treeViewSetRubberBanding tv False

    return (model, filterModel)

  where
    createColumn m (name, width, attr) = do
        col <- treeViewColumnNew
        treeViewColumnSetFixedWidth col width
        treeViewColumnSetSizing col TreeViewColumnSizingFixed
        treeViewColumnSetResizable col True
        treeViewColumnSetReorderable col True
        treeViewColumnSetTitle col name
        renderer <- cellRendererTextNew
        cellLayoutPackStart col renderer True
        cellLayoutSetAttributes col renderer m attr
        void $ treeViewAppendColumn tv col
        return (name, col, renderer)

    filterFunction m entry func _ iter = do
        idx  <- seqStoreIterToIndex iter
        val  <- seqStoreGetValue m idx
        text <- get entry #text
        let searchText = T.toLower text
            !res = func searchText val 
        return res



type SortFunc a
    =  SeqStore a
    -> TreeModelSort
    -> TreeModel
    -> TreeIter
    -> TreeIter
    -> IO Int32

sortFuncGetValues
    :: SeqStore a
    -> TreeModelSort
    -> (a -> a -> Ordering)
    -> TreeModel
    -> TreeIter
    -> TreeIter
    -> IO Int32
sortFuncGetValues model _sortModel comparisonFunc _ iter1 iter2 = do
    idx1 <- seqStoreIterToIndex iter1
    idx2 <- seqStoreIterToIndex iter2

    val1 <- seqStoreGetValue model idx1
    val2 <- seqStoreGetValue model idx2

    case comparisonFunc val1 val2 of
        LT -> return (-1)
        EQ -> return 0
        GT -> return 1


createSortedScrollingTable
    :: TreeView
    -> [ ( Text
         , Int32
         , Maybe (Int32, a -> a -> Ordering)
         , a -> [AttrOp CellRendererText 'AttrSet]
         )
       ]
    -> IO (TreeView, SeqStore a, TreeModelSort)
createSortedScrollingTable tv attribs = do
    (model, sort) <- createSortedScrollingTableSimple tv attribs
    return (tv, model, sort)


createSortedScrollingTableSimple
    :: TreeView
    -> [ ( Text
         , Int32
         , Maybe (Int32, row -> row -> Ordering)
         , row -> [AttrOp CellRendererText 'AttrSet]
         )
       ]
    -> IO (SeqStore row, TreeModelSort)
createSortedScrollingTableSimple tv attribs = do
    model     <- seqStoreNew []
    sortModel <- new TreeModelSort [#model := model] 
    
    treeViewSetModel tv (Just sortModel)

    treeViewSetHeadersVisible tv True

    mapM_ (createColumn model sortModel) attribs

    -- try to set fixed height mode for more speed
    treeViewSetFixedHeightMode tv True

    return (model, sortModel)

  where
    createColumn model sortModel (name, width, sorting, attr) = do
        col <- treeViewColumnNew
        treeViewColumnSetFixedWidth col width
        treeViewColumnSetSizing col TreeViewColumnSizingFixed
        treeViewColumnSetResizable col True
        treeViewColumnSetReorderable col True
        treeViewColumnSetTitle col name
        case sorting of
            Just (colId, compareFunc) -> do
                treeViewColumnSetSortColumnId col colId
                treeViewColumnSetSortIndicator col True
                treeViewColumnSetSortOrder col SortTypeDescending
                treeSortableSetSortFunc
                    sortModel
                    colId
                    (sortFuncGetValues model sortModel compareFunc)
            Nothing -> return ()
        renderer <- cellRendererTextNew
        cellLayoutPackStart col renderer True
        cellLayoutSetAttributes col renderer model attr
        void $ treeViewAppendColumn tv col
        return (name, col, renderer)




