module GUI.TreeView
(scrollToTop)
where


import RIO
import GI.Gtk



scrollToTop :: (IsTreeView a) => a -> IO () 
scrollToTop tv = do 
  row <- treePathNewFromIndices [0]
  treeViewScrollToCell tv (Just row) (Nothing :: Maybe TreeViewColumn)  False 0 0 