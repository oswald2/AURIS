module GUI.MessageDetails
    ( createMsgDetailWindow
    , MessageDetailWindow
    , msgDetailShowMsg
    , msgDetailShowWindow
    ) where


import           RIO

import           GI.Gtk                        as Gtk

import           GUI.MessageTypes
import           GUI.Utils
import           GUI.TextView
import           GUI.StatusEntry



data MessageDetailWindow = MessageDetailWindow
    { _msgDetParent      :: !ApplicationWindow
    , _msgDetWindow      :: !Window
    , _msgDetEntryTime   :: !Entry
    , _mstDetEntryLevel  :: !StatusEntry
    , _mstDetEntrySource :: !Entry
    , _mstDetTextView    :: !TextView
    }


createMsgDetailWindow :: ApplicationWindow -> Gtk.Builder -> IO MessageDetailWindow
createMsgDetailWindow window builder = do
    win      <- getObject builder "windowMessageDetail" Window
    textView <- getObject builder "textViewMessageDetail" TextView
    eTime    <- getObject builder "entryMsgDetailTime" Entry
    eLevel   <- getObject builder "entryMsgDetailLevel" Entry
    eSource  <- getObject builder "entryMsgDetailSource" Entry

    seLevel  <- statusEntrySetupCSS eLevel

    let gui = MessageDetailWindow { _msgDetParent      = window
                                  , _msgDetWindow      = win
                                  , _msgDetEntryTime   = eTime
                                  , _mstDetEntryLevel  = seLevel
                                  , _mstDetEntrySource = eSource
                                  , _mstDetTextView    = textView
                                  }

    void $ Gtk.on win #deleteEvent $ \_ -> do
        widgetHide win
        return True

    return gui



msgDetailShowWindow :: MessageDetailWindow -> IO ()
msgDetailShowWindow gui = do
    widgetShowAll (_msgDetWindow gui)

msgDetailShowMsg :: MessageDetailWindow -> MessageEntry -> IO ()
msgDetailShowMsg gui (MessageEntry t level source content) = do
    entrySetText (_msgDetEntryTime gui) (textDisplay t)
    statusEntrySetState (_mstDetEntryLevel gui)
                        (levelStatus level)
                        (levelText level)
    entrySetText (_mstDetEntrySource gui) source
    textViewSetText (_mstDetTextView gui) content
  where
    levelText LevelDebug       = "DEBUG"
    levelText LevelInfo        = "INFO"
    levelText LevelWarn        = "WARN"
    levelText LevelError       = "ERROR"
    levelText (LevelOther lvl) = "Other: " <> textDisplay lvl

    levelStatus LevelDebug   = ESGreen
    levelStatus LevelInfo    = ESGreen
    levelStatus LevelWarn    = ESWarn
    levelStatus LevelError   = ESError
    levelStatus LevelOther{} = ESWarn
