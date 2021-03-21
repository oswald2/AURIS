{-# OPTIONS_GHC -fno-warn-orphans #-}
module GUI.Reactive.TextView where

import           RIO

import           GI.Gtk                        as Gtk
import           Data.GI.Gtk.Threading

import           Data.ReactiveValue

import           GUI.TextView


textViewTextReactive :: TextView -> ReactiveFieldReadWrite IO Text
textViewTextReactive tv = ReactiveFieldReadWrite setter getter notifier
  where
    getter = textViewGetText tv
    setter v = postGUIASync $ textViewSetText tv v
    notifier f = do
        buf <- textViewGetBuffer tv
        void (Gtk.on buf #changed f)
