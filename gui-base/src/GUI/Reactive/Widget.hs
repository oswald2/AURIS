{-# OPTIONS_GHC -fno-warn-orphans #-}
module GUI.Reactive.Widget where

import           RIO

import           GI.Gtk                        as Gtk
import           Data.GI.Gtk.Threading          ( postGUIASync )

import           Data.ReactiveValue



widgetSensitiveReactive :: Widget -> ReactiveFieldReadWrite IO Bool
widgetSensitiveReactive w = ReactiveFieldReadWrite setter getter notifier
  where
    setter v = postGUIASync $ do
        val <- widgetGetSensitive w 
        when (val /= v) $ widgetSetSensitive w v
    getter = widgetGetSensitive w 
    notifier cb = void (Gtk.on w #mapEvent $ \_ -> cb >> return False)


