module GUI.ConnectionStatus
  ( ConnectionStatus
  , ConnectionState(..)
  , newConnectionStatus
  , setConnectionState
  )
where

import           RIO


import           GI.Gtk                        as Gtk

import           GUI.StatusEntry

import           Protocol.ProtocolInterfaces



data ConnectionState = 
  Accepting
  | Connected 
  | Disconnected


data ConnectionStatus = ConnectionStatus {
  _connStatEntry :: StatusEntry
  , _connStatFrame :: Frame
  , _connStatGrid :: Grid
  , _connStatInnerBox :: Box
  , _connStatHost :: Label
  , _connStatPort :: Label
  }


newConnectionStatus
  :: ProtocolInterface -> Text -> Word16 -> IO ConnectionStatus
newConnectionStatus interface host port = do

  frame     <- frameNew (Just (interf interface))
  grid      <- gridNew

  hostlabel <- labelNew (Just "Host:")
  hostl     <- labelNew (Just host)
  portlabel <- labelNew (Just "Port: ")
  portl     <- labelNew (Just (textDisplay port))

  gridAttach grid hostlabel 0 0 1 1
  gridAttach grid hostl     1 0 1 1
  gridAttach grid portlabel 0 1 1 1
  gridAttach grid portl     1 1 1 1

  entry <- entryNew
  status <- statusEntrySetupCSS entry

  box <- boxNew OrientationHorizontal 0 
  boxPackStart box grid False False 5 
  boxPackStart box entry True True 5 

  containerAdd frame box

  let g = ConnectionStatus { _connStatEntry    = status
                           , _connStatFrame    = frame
                           , _connStatGrid     = grid
                           , _connStatInnerBox = box
                           , _connStatHost     = hostl
                           , _connStatPort     = portl
                           }
  return g

 where
  interf (IfNctrs    x) = "NCTRS " <> textDisplay x
  interf (IfCnc      x) = "C&C " <> textDisplay x
  interf (IfEden     x) = "EDEN " <> textDisplay x
  interf (IfEdenScoe x) = "EDEN SCOE " <> textDisplay x


setConnectionState :: ConnectionStatus -> ConnectionState -> IO () 
setConnectionState ConnectionStatus {..} Accepting = statusEntrySetState _connStatEntry ESWarn "ACCEPTING"
setConnectionState ConnectionStatus {..} Disconnected = statusEntrySetState _connStatEntry ESError "DISCONNECTED"
setConnectionState ConnectionStatus {..} Connected = statusEntrySetState _connStatEntry ESGreen "CONNECTED"
