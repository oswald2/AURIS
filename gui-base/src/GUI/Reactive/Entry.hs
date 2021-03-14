{-# OPTIONS_GHC -fno-warn-orphans #-}
module GUI.Reactive.Entry where

import           RIO

import           GI.Gtk                        as Gtk
import           Data.GI.Gtk.Threading

import           Data.ReactiveValue             ( ReactiveFieldReadWrite
                                                , ReactiveValueRead(..)
                                                , ReactiveValueReadWrite
                                                , ReactiveValueWrite(..),ReactiveFieldWrite (ReactiveFieldWrite)
                                                )

import           GUI.Reactive.Property



entryTextReactive :: Entry -> ReactiveFieldReadWrite IO Text
entryTextReactive e = reactiveProperty e #changed #text


entryTextWO :: Entry -> ReactiveFieldWrite IO Text 
entryTextWO e = ReactiveFieldWrite setter 
    where 
        setter v = postGUIASync $ entrySetText e v


instance ReactiveValueReadWrite Entry Text IO


instance ReactiveValueRead Entry Text IO where
    reactiveValueRead      = reactiveValueRead . entryTextReactive
    reactiveValueOnCanRead = reactiveValueOnCanRead . entryTextReactive

instance ReactiveValueWrite Entry Text IO where
    reactiveValueWrite = reactiveValueWrite . entryTextReactive

