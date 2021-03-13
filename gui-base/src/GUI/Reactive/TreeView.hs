{-# OPTIONS_GHC -fno-warn-orphans #-}
module GUI.Reactive.TreeView where

import           RIO

import           GI.Gtk                        as Gtk
import           Data.GI.Gtk.ModelView.SeqStore

import           Data.ReactiveValue



treeViewSelectedRowReactive
    :: TreeView -> SeqStore a -> ReactiveFieldRead IO (Maybe a)
treeViewSelectedRowReactive tv model = ReactiveFieldRead getter notifier
  where
    getter = do
        sel        <- treeViewGetSelection tv
        (paths, _) <- treeSelectionGetSelectedRows sel
        case paths of
            (x : _) -> do
                ix <- treePathGetIndices x
                case ix of
                    Just (i : _) -> do
                        Just <$> seqStoreGetValue model i
                    _ -> return Nothing
            _ -> return Nothing

    notifier cb = void (Gtk.on tv #rowActivated $ \_ _ -> cb)


treeViewSelectedRowsReactive
    :: TreeView -> SeqStore a -> ReactiveFieldRead IO [a]
treeViewSelectedRowsReactive tv model = ReactiveFieldRead getter notifier
  where
    getter = do
        sel        <- treeViewGetSelection tv
        (paths, _) <- treeSelectionGetSelectedRows sel
        getRows paths
    getRows paths = concat <$> mapM getFromPath paths
    getFromPath path = do
      ix <- treePathGetIndices path
      case ix of
          Just (i : _) -> (:[]) <$> seqStoreGetValue model i
          _ -> return []

    notifier cb = void (Gtk.on tv #rowActivated $ \_ _ -> cb)
