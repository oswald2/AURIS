module GUI.TextView
  ( textViewSetText
  , textViewSetTextMarkup
  , textViewClear
  , textViewGetText
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

textViewSetTextMarkup :: TextView -> Text -> IO () 
textViewSetTextMarkup tv txt = do 
  buf <- textViewGetBuffer tv 
  (start, end) <- textBufferGetBounds buf
  textBufferDelete buf start end 
  textBufferInsertMarkup buf start txt (fromIntegral (T.length txt))



textViewGetText :: TextView -> IO Text
textViewGetText tv = do 
  buffer       <- textViewGetBuffer tv
  (start, end) <- textBufferGetBounds buffer
  textBufferGetText buffer start end False



textViewClear :: TextView -> IO ()
textViewClear tv = do 
  buffer <- textViewGetBuffer tv
  (start, end) <- textBufferGetBounds buffer 
  textBufferDelete buffer start end 
