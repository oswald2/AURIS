module GUI.MessageDisplay
  ( addMessageLine
  , addMessageLine'
  , messageAreaLogFunc
  )
where

import           RIO
import qualified RIO.Text                      as T
import           Graphics.UI.FLTK.LowLevel.FLTKHS
                                         hiding ( display )

import           GUI.MainWindow
import           GUI.Utils

import           General.Time

-- mkLogFunc :: (CallStack -> LogSource -> LogLevel -> Utf8Builder -> IO ()) -> LogFunc

messageAreaLogFunc :: MainWindow -> LogFunc
messageAreaLogFunc window = mkLogFunc (addMessageLine window)

maxMsgs :: Int
maxMsgs = 200


addMessageLine
  :: MainWindow -> CallStack -> LogSource -> LogLevel -> Utf8Builder -> IO ()
addMessageLine window _stack = addMessageLine' window

addMessageLine'
  :: MainWindow -> LogSource -> LogLevel -> Utf8Builder -> IO ()
addMessageLine' window source level builder = do
  now <- getCurrentTime

  let src ::Utf8Builder
      src = if T.null source
        then display T.empty
        else display (" [" :: Text) <> display source <> display ("] " :: Text)


  case level of
    LevelDebug -> return ()
    LevelInfo  -> do
      let text =
            utf8BuilderToText
              $  display ("@C61@." :: Text)
              <> display now
              <> display (": " :: Text)
              <> src
              <> builder
      dispMsg text
    LevelWarn -> do
      let text =
            utf8BuilderToText
              $  display ("@B3@C0@." :: Text)
              <> display now
              <> display (": " :: Text)
              <> src
              <> builder
      dispMsg text
    LevelError -> do
      let text =
            utf8BuilderToText
              $  display ("@B1@C7@." :: Text)
              <> display now
              <> display (": " :: Text)
              <> src
              <> builder
      dispMsg text
    LevelOther lvl -> do
      let text = utf8BuilderToText $ display lvl <> display source <> builder
      dispMsg text

  where
    dispMsg text = do
      let msgDisplay = window ^. mwMessageDisplay
      withFLLock $ do
        size <- getSize msgDisplay
        when (size > maxMsgs) $ remove msgDisplay (LineNumber 1)
        mapM_ (add msgDisplay) (T.lines text)

