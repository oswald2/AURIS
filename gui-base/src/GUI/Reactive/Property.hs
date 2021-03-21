{-# OPTIONS_GHC -fno-warn-orphans #-}
module GUI.Reactive.Property where

import           RIO

import           GI.Gtk                        as Gtk
import           Data.GI.Gtk.Threading
import           Data.GI.Base.Attributes
import           Data.GI.Base.Signals

import           Data.ReactiveValue



reactiveProperty
    :: ( Eq result
       , AttrGetC i self attr result
       , AttrSetC i self attr result
       , GObject self
       , SignalInfo info
       , HaskellCallbackType info ~ IO ()
       )
    => self
    -> SignalProxy self info
    -> AttrLabelProxy attr
    -> ReactiveFieldReadWrite IO result
reactiveProperty self signal attr = ReactiveFieldReadWrite setter
                                                           getter
                                                           notifier
  where
    setter v = postGUIASync $ do
        val <- getter
        when (val /= v) $ Gtk.set self [attr := v]
    getter = get self attr
    notifier cb = void (Gtk.on self signal cb)


reactivePropertyNE
    :: ( AttrGetC i self attr result
       , AttrSetC i self attr result
       , GObject self
       , SignalInfo info
       , HaskellCallbackType info ~ IO ()
       )
    => self
    -> SignalProxy self info
    -> AttrLabelProxy attr
    -> ReactiveFieldReadWrite IO result
reactivePropertyNE self signal attr = ReactiveFieldReadWrite setter
                                                             getter
                                                             notifier
  where
    setter v = postGUIASync $ Gtk.set self [attr := v]
    getter = get self attr
    notifier cb = void (Gtk.on self signal cb)



reactivePropertyH
    :: ( AttrGetC i self attr result
       , AttrSetC i self attr result
       )
    => self
    -> (self -> IO () -> IO a)
    -> AttrLabelProxy attr
    -> ReactiveFieldReadWrite IO result
reactivePropertyH self signal attr = ReactiveFieldReadWrite setter
                                                             getter
                                                             notifier
  where
    setter v = postGUIASync $ Gtk.set self [attr := v]
    getter = get self attr
    notifier cb = void (self `signal` cb)



reactiveSignalIO
    :: (SignalInfo info, GObject self, HaskellCallbackType info ~ IO ())
    => self
    -> SignalProxy self info
    -> ReactiveFieldRead IO ()
reactiveSignalIO self signal = ReactiveFieldRead getter notifier
  where
    getter = return ()
    notifier cb = void (Gtk.on self signal cb)



reactiveSignalM
    :: (SignalInfo (m a), GObject self, HaskellCallbackType (m a) ~ IO a)
    => self
    -> a
    -> SignalProxy self (m a)
    -> ReactiveFieldRead IO ()
reactiveSignalM self def signal = ReactiveFieldRead getter notifier
  where
    getter = return ()
    notifier cb = void (Gtk.on self signal (liftIO cb >> return def))




passiveProperty
    :: (Eq result, AttrGetC i self attr result, AttrSetC i self attr result)
    => self
    -> AttrLabelProxy attr
    -> ReactiveFieldReadWrite IO result
passiveProperty self attr = ReactiveFieldReadWrite setter getter notifier
  where
    setter v = postGUIASync $ do
        val <- getter
        when (val /= v) $ Gtk.set self [attr := v]
    getter = get self attr
    notifier _cb = return ()


passivePropertyNE
    :: (AttrGetC i self attr result, AttrSetC i self attr result)
    => self
    -> AttrLabelProxy attr
    -> ReactiveFieldReadWrite IO result
passivePropertyNE self attr = ReactiveFieldReadWrite setter getter notifier
  where
    setter v = postGUIASync $ Gtk.set self [attr := v]
    getter = get self attr
    notifier _cb = return ()
