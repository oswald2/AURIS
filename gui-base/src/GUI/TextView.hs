module GUI.TextView
  ( textViewSetText
  )
where

import           RIO
import qualified RIO.Text                      as T
import           GI.Gtk                        as Gtk


-- | Convenience function to set a text in a 'TextView'
textViewSetText :: TextView -> Text -> IO ()
textViewSetText tv txt = do
  buf <- textViewGetBuffer tv
  textBufferSetText buf txt (fromIntegral (T.length txt))
