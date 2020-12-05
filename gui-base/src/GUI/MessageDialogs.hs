module GUI.MessageDialogs
  ( warningDialog
  , infoDialog
  , errorDialog
  )
where

import           RIO

import           GI.Gtk                        as Gtk



infoDialog :: Text -> IO ()
infoDialog msg = do
  diag <- new
    MessageDialog
    [ #text := msg
    , #buttons := ButtonsTypeOk
    , #messageType := MessageTypeInfo
    ]
  _ <- dialogRun diag
  return ()


warningDialog :: Text -> IO ()
warningDialog msg = do
  diag <- new
    MessageDialog
    [ #text := msg
    , #buttons := ButtonsTypeOk
    , #messageType := MessageTypeWarning
    ]
  _ <- dialogRun diag
  widgetHide diag
  return ()


errorDialog :: Text -> IO ()
errorDialog msg = do
  diag <- new
    MessageDialog
    [ #text := msg
    , #buttons := ButtonsTypeOk
    , #messageType := MessageTypeError
    ]
  _ <- dialogRun diag
  return ()
