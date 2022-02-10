module GUI.MessageDisplay
    ( MessageDisplay
    , MessageEntry(..)
    , addMessageLine
    , addMessageLine'
    , messageAreaLogFunc
    , createMessageDisplay
    ) where

import           RIO

import           GI.Gtk                        as Gtk
import           Data.GI.Gtk.ModelView.SeqStore
import           Data.GI.Gtk.Threading

import           GUI.Utils
import           GUI.Colors
import           GUI.ScrollingTable
import           GUI.MessageTypes
import           GUI.MessageDetails

import           General.Time






data MessageDisplay = MessageDisplay
    { _msgdDetails :: !MessageDetailWindow
    , _msgdDisplay :: !TreeView
    , _msgdModel   :: SeqStore MessageEntry
    }


createMessageDisplay :: MessageDetailWindow -> Gtk.Builder -> IO MessageDisplay
createMessageDisplay !detWin !builder = do
    tv  <- getObject builder "messageDisplay" TreeView

    gui <- createScrollingTable
        tv
        (MessageDisplay detWin)
        [ ("Time"   , 190, timeAttrs)
        , ("Level"  , 50 , levelAttrs)
        , ("Source" , 70 , sourceAttrs)
        , ("Message", 800, textAttrs)
        ]
    setTreeViewCallback gui _msgdDisplay _msgdModel (displayMessage gui)
    return gui

  where
    timeAttrs (MessageEntry time _ _ _) = [#text := textDisplay time]
    levelAttrs (MessageEntry _ (LevelOther lvl) _ _) =
        (#text := utf8BuilderToText (display ("Other: " :: Text) <> display lvl)
            )
            : colors (LevelOther lvl)
    levelAttrs (MessageEntry _ LevelDebug _ _) =
        (#text := ("DEBUG" :: Text)) : colors LevelDebug
    levelAttrs (MessageEntry _ LevelInfo _ _) =
        (#text := ("INFO" :: Text)) : colors LevelInfo
    levelAttrs (MessageEntry _ LevelWarn _ _) =
        (#text := ("WARN" :: Text)) : colors LevelWarn
    levelAttrs (MessageEntry _ LevelError _ _) =
        (#text := ("ERROR" :: Text)) : colors LevelError
    sourceAttrs (MessageEntry _ _ source _) = [#text := source]
    textAttrs (MessageEntry _ _ _ text) = [#text := text]

    colors LevelDebug = [#backgroundSet := False, #foregroundSet := False]
    -- colors LevelInfo  = [#backgroundSet := False, #foregroundSet := False]
    colors LevelInfo =
        [ #backgroundSet := True
        , #foregroundSet := True
        , #backgroundRgba := green
        , #foregroundRgba := black
        ]
    colors LevelWarn =
        [ #backgroundSet := True
        , #foregroundSet := True
        , #backgroundRgba := paleYellow
        , #foregroundRgba := black
        ]
    colors LevelError =
        [ #backgroundSet := True
        , #foregroundSet := True
        , #backgroundRgba := red
        , #foregroundRgba := white
        ]
    colors (LevelOther _) = [#backgroundSet := False, #foregroundSet := False]

messageAreaLogFunc :: MessageDisplay -> LogFunc
messageAreaLogFunc window = mkLogFunc (addMessageLine window)

--maxMsgs :: Int
--maxMsgs = 200


addMessageLine
    :: MessageDisplay
    -> CallStack
    -> LogSource
    -> LogLevel
    -> Utf8Builder
    -> IO ()
addMessageLine window _stack = addMessageLine' window

addMessageLine'
    :: MessageDisplay -> LogSource -> LogLevel -> Utf8Builder -> IO ()
addMessageLine' _      _      LevelDebug _       = return ()
addMessageLine' window source level      builder = do
    now <- getCurrentTime
    postGUIASync $ do
        addRowScrollingTable
            (_msgdDisplay window)
            (_msgdModel window)
            (MessageEntry now level source (utf8BuilderToText builder))



displayMessage :: MessageDisplay -> MessageEntry -> IO ()
displayMessage gui entry = do 
  msgDetailShowMsg (_msgdDetails gui) entry
  msgDetailShowWindow (_msgdDetails gui)


