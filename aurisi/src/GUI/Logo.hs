{-# LANGUAGE
  TemplateHaskell
  , ForeignFunctionInterface
#-}
module GUI.Logo
  ( setLogo
  , getLogoPixbuf
  )
where


import           RIO
--import qualified Data.Text.IO                  as T
import           GI.Gtk                        as Gtk
import           GI.GdkPixbuf.Objects.Pixbuf


setLogo :: Image -> Int32 -> Int32 -> IO ()
setLogo img width height = do
  pixbuf <- pixbufNewFromResourceAtScale "/auris/data/AurisLogo.svg" width height True
  imageSetFromPixbuf img (Just pixbuf)



getLogoPixbuf :: Int32 -> Int32 -> IO Pixbuf
getLogoPixbuf width height = do
  pixbufNewFromResourceAtScale "/auris/data/AurisLogo.svg" width height True
