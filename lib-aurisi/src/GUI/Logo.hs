{-# LANGUAGE
  TemplateHaskell
  , ForeignFunctionInterface
#-}
module GUI.Logo
    ( setLogo
    , getLogoPixbuf
    ) where


import           RIO
import           GI.Gtk                        as Gtk
                                                ( imageSetFromPixbuf
                                                , Image
                                                )
import           GI.GdkPixbuf.Objects.Pixbuf    ( pixbufNewFromResourceAtScale
                                                , Pixbuf
                                                )


setLogo :: Image -> Int32 -> Int32 -> IO ()
setLogo img width height = do
    pixbuf <- pixbufNewFromResourceAtScale "/auris/data/AurisLogo.svg"
                                           width
                                           height
                                           True
    imageSetFromPixbuf img pixbuf



getLogoPixbuf :: Int32 -> Int32 -> IO (Maybe Pixbuf)
getLogoPixbuf width height = do
    pixbufNewFromResourceAtScale "/auris/data/AurisLogo.svg" width height True
