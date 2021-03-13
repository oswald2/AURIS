{-# OPTIONS_GHC -fno-warn-orphans #-}
module GUI.Reactive.ToggleButton where


import           RIO

import           GI.Gtk                        as Gtk

import           Data.ReactiveValue


toggleButtonActiveReactive :: ToggleButton -> ReactiveFieldReadWrite IO Bool
toggleButtonActiveReactive t = ReactiveFieldReadWrite
    (toggleButtonSetActive t)
    (toggleButtonGetActive t)
    notifier
  where 
    notifier f = void (Gtk.on t #toggled f)


checkButtonActiveReactive :: CheckButton -> ReactiveFieldReadWrite IO Bool
checkButtonActiveReactive t = ReactiveFieldReadWrite
    (toggleButtonSetActive t)
    (toggleButtonGetActive t)
    notifier
  where 
    notifier f = void (Gtk.on t #toggled f)
