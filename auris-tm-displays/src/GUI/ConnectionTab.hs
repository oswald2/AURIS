module GUI.ConnectionTab
  ( ConnectionTab(..)
  , initConnectionTab
  )
where

import           RIO

import           GI.Gtk                        as Gtk

import           GUI.ConnectionStatus

import           Protocol.ProtocolInterfaces
import           Data.PUS.Config


data ConnectionTab = ConnectionTab {
  _connTabEntries :: HashMap ProtocolInterface ConnectionStatus
  , _connTabBox :: Box
  }


initConnectionTab :: Config -> Gtk.Builder -> IO ConnectionTab
initConnectionTab config builder = return undefined

