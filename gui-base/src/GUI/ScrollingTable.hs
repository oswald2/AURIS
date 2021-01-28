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
  , setTreeViewCallback
  , createScrollingTable
  )
where

import           RIO

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
setRowsSeqStore :: SeqStore a -> [a] -> IO ()
setRowsSeqStore model values = do
  seqStoreClear model
  mapM_ (seqStorePrepend model) values


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
  model <- seqStoreNew []

  treeViewSetModel tv (Just model)

  treeViewSetHeadersVisible tv True

  mapM_ (createColumn model) attribs

  -- try to set fixed height mode for more speed
  treeViewSetFixedHeightMode tv True 

  return $ constr tv model

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





