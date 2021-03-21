
{-# OPTIONS_GHC -fno-warn-orphans #-}
module GUI.Reactive.Button
where 

import RIO

import GI.Gtk as Gtk

import Data.ReactiveValue


buttonActivateField :: Button -> ReactiveFieldActivatable IO 
buttonActivateField b = mkActivatable op 
  where 
    op f = void (Gtk.on b #clicked f)



instance ReactiveValueActivatable IO Button where 
  defaultActivation = buttonActivateField