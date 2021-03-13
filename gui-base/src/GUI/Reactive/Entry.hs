{-# OPTIONS_GHC -fno-warn-orphans #-}
module GUI.Reactive.Entry where

import           RIO

import           GI.Gtk                        as Gtk

import           Data.ReactiveValue             ( ReactiveFieldReadWrite
                                                , ReactiveValueRead(..)
                                                , ReactiveValueReadWrite
                                                , ReactiveValueWrite(..)
                                                )

import           GUI.Reactive.Property



entryTextReactive :: Entry -> ReactiveFieldReadWrite IO Text
entryTextReactive e = reactiveProperty e #changed #text


instance ReactiveValueReadWrite Entry Text IO


instance ReactiveValueRead Entry Text IO where
    reactiveValueRead      = reactiveValueRead . entryTextReactive
    reactiveValueOnCanRead = reactiveValueOnCanRead . entryTextReactive

instance ReactiveValueWrite Entry Text IO where
    reactiveValueWrite = reactiveValueWrite . entryTextReactive

