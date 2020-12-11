module GUI.Definitions
  ( defMaxRowTM
  , setEntryStyle
  ) where


import           RIO

import           GI.Gtk                        as Gtk
                                                ( cssProviderLoadFromData
                                                , cssProviderNew
                                                )
import           GI.Gtk.Objects.StyleContext    ( styleContextAddProviderForScreen
                                                )
import           GI.Gdk.Objects.Screen          ( screenGetDefault )


-- | The maximum number of rows in a TM display in live mode
defMaxRowTM :: Int32
defMaxRowTM = 200



css :: ByteString
css = "entry { min-height: 0px; }"


setEntryStyle :: (MonadIO m) => m ()
setEntryStyle = do
  provider <- cssProviderNew
  cssProviderLoadFromData provider css
  r <- screenGetDefault
  case r of
    Nothing   -> return ()
    Just disp -> styleContextAddProviderForScreen disp provider 600
