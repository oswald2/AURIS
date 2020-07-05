{-# LANGUAGE
    OverloadedStrings
    , BangPatterns
    , NoImplicitPrelude
#-}
module GUI.Utils
  ( getObject
  , withFLLock
  )
where


import           RIO
import qualified RIO.Text                      as T
import           GI.Gtk                        as Gtk


getObject :: GObject o => Gtk.Builder -> Text -> (ManagedPtr o -> o) -> IO o
getObject builder obj gtkConstr = do
  o <- builderGetObject builder obj
  case o of
    Nothing ->
      error $ "GTK: could not find " <> T.unpack obj <> " in Glade file!"
    Just oo -> do
      w <- castTo gtkConstr oo
      case w of
        Nothing     -> error $ "GTK: cannot cast widget" <> T.unpack obj
        Just widget -> return widget



withFLLock _ = return ()