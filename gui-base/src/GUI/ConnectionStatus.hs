module GUI.ConnectionStatus
  ( ConnectionStatus
  , newConnectionStatus
  , setConnectionState
  , connStatFrame
  )
where

import           RIO


import           GI.Gtk                        as Gtk

import           GUI.StatusEntry

import           Protocol.ProtocolInterfaces




data ConnectionStatus = ConnectionStatus {
  _connStatEntry :: StatusEntry
  , _connStatFrame :: Frame
  , _connStatGrid :: Grid
  , _connStatInnerBox :: Box
  , _connStatHost :: Label
  , _connStatPort :: Label
  }


connStatFrame :: ConnectionStatus -> Frame
connStatFrame = _connStatFrame


newConnectionStatus
  :: ProtocolInterface -> Text -> Text -> Word16 -> IO ConnectionStatus
newConnectionStatus interface connType host port = do

  frame <- frameNew (Just (interf interface connType))
  t     <- getFrameLabelWidget frame 
  case t of
    Nothing -> return ()
    Just t2 -> do
      castTo Label t2 >>= \case
        Nothing -> return () 
        Just titleLabel -> labelSetUseMarkup titleLabel True

  grid      <- gridNew

  hostlabel <- labelNew (Just "Host:")
  hostl     <- labelNew (Just host)
  portlabel <- labelNew (Just "Port: ")
  portl     <- labelNew (Just (textDisplay port))

  gridAttach grid hostlabel 0 0 1 1
  gridAttach grid hostl     1 0 1 1
  gridAttach grid portlabel 0 1 1 1
  gridAttach grid portl     1 1 1 1

  entry  <- entryNew
  status <- statusEntrySetupCSS entry
  entrySetAlignment entry 0.5

  box <- boxNew OrientationHorizontal 0
  boxPackStart box grid  False False 5
  boxPackStart box entry True  True  5

  containerAdd frame box

  let g = ConnectionStatus { _connStatEntry    = status
                           , _connStatFrame    = frame
                           , _connStatGrid     = grid
                           , _connStatInnerBox = box
                           , _connStatHost     = hostl
                           , _connStatPort     = portl
                           }

  setConnectionState g Disconnected

  return g

 where
  interf (IfNctrs    x) t = "<b>NCTRS " <> textDisplay x <> " (" <> t <> ")</b>"
  interf (IfCnc      x) t = "<b>CnC " <> textDisplay x <> " (" <> t <> ")</b>"
  interf (IfEden     x) _ = "<b>EDEN " <> textDisplay x <> "</b>"


setConnectionState :: ConnectionStatus -> ConnectionState -> IO ()
setConnectionState ConnectionStatus {..} Accepting =
  statusEntrySetState _connStatEntry ESWarn "ACCEPTING"
setConnectionState ConnectionStatus {..} Disconnected =
  statusEntrySetState _connStatEntry ESError "DISCONNECTED"
setConnectionState ConnectionStatus {..} Connected =
  statusEntrySetState _connStatEntry ESGreen "CONNECTED"
