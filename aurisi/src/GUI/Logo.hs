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
--import           GI.GdkPixbuf.Enums

--import           GUI.LogoXPM


-- setLogo :: Image -> Int32 -> Int32 -> IO ()
-- setLogo img width height = do
--   pixbuf <- pixbufNewFromXpmData aurisLogoXPM
--   res    <- pixbufScaleSimple pixbuf width height InterpTypeHyper
--   case res of
--     Just p  -> imageSetFromPixbuf img (Just p)
--     Nothing -> error "Could not scale logo!"

setLogo :: Image -> Int32 -> Int32 -> IO ()
setLogo img width height = do
  pixbuf <- pixbufNewFromResourceAtScale "/auris/src/AurisLogo.svg" width height True
  imageSetFromPixbuf img (Just pixbuf)



-- getLogoPixbuf :: Int32 -> Int32 -> IO Pixbuf
-- getLogoPixbuf width height = do
--   pixbuf <- pixbufNewFromXpmData aurisLogoXPM
--   res    <- pixbufScaleSimple pixbuf width height InterpTypeHyper
--   case res of
--     Just p  -> return p
--     Nothing -> error "Could not scale logo!"

getLogoPixbuf :: Int32 -> Int32 -> IO Pixbuf
getLogoPixbuf width height = do
  pixbufNewFromResourceAtScale "/auris/src/AurisLogo.svg" width height True
