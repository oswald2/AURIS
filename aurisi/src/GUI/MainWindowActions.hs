module GUI.MainWindowActions
  ( mwLogInfo
  , mwLogWarn
  , mwLogAlarm
  )
where


import           RIO

import           GUI.MainWindow
import           GUI.MessageDisplay


mwLogInfo :: MainWindow -> Text -> IO ()
mwLogInfo window txt =
  addMessageLine' (window ^. mwMessageDisplay) "PACKET" LevelInfo (display txt)

mwLogWarn :: MainWindow -> Text -> IO ()
mwLogWarn window txt =
  addMessageLine' (window ^. mwMessageDisplay) "PACKET" LevelWarn (display txt)


mwLogAlarm :: MainWindow -> Text -> IO ()
mwLogAlarm window txt =
  addMessageLine' (window ^. mwMessageDisplay) "PACKET" LevelError (display txt)
