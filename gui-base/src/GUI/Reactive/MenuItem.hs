
{-# OPTIONS_GHC -fno-warn-orphans #-}
module GUI.Reactive.MenuItem
where 

import RIO

import GI.Gtk as Gtk

import Data.ReactiveValue


menuItemActivateField :: MenuItem -> ReactiveFieldActivatable IO 
menuItemActivateField b = mkActivatable op 
  where 
    op f = void (Gtk.on b #activate f)



instance ReactiveValueActivatable IO MenuItem where 
  defaultActivation = menuItemActivateField