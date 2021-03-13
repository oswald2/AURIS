{-# OPTIONS_GHC -fno-warn-orphans #-}
module GUI.Reactive.SpinButton where

import           RIO

import           GI.Gtk                        as Gtk
import           Data.GI.Gtk.Threading

import           Data.ReactiveValue




spinButtonValueInt :: SpinButton -> ReactiveFieldReadWrite IO Int
spinButtonValueInt s = ReactiveFieldReadWrite setter getter notifier 
    where 
      setter v = postGUIASync $ do 
        val <- spinButtonGetValue s
        when (v /= round val) $ spinButtonSetValue s (fromIntegral v)
      getter = round <$> spinButtonGetValue s
      notifier f = void (Gtk.on s #valueChanged f)

instance ReactiveValueReadWrite SpinButton Int IO


instance ReactiveValueRead SpinButton Int IO where
    reactiveValueRead      = reactiveValueRead . spinButtonValueInt
    reactiveValueOnCanRead = reactiveValueOnCanRead . spinButtonValueInt

instance ReactiveValueWrite SpinButton Int IO where
    reactiveValueWrite = reactiveValueWrite . spinButtonValueInt

