{-# LANGUAGE
    OverloadedStrings
    , BangPatterns
    , NoImplicitPrelude
#-}
module GUI.Utils
    ( getObject
    , convertRGBA
    , convertColour
    ) where


import           Data.Colour
import           Data.Colour.SRGB

import           GI.Gdk.Structs.RGBA
import           GI.Gtk                        as Gtk
import           RIO
import qualified RIO.Text                      as T


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




convertRGBA :: RGBA -> IO (Colour Double)
convertRGBA rgba = do
    r <- getRGBARed rgba
    g <- getRGBAGreen rgba
    b <- getRGBABlue rgba
    pure $ (sRGB r g b)


convertColour :: Colour Double -> IO RGBA
convertColour c = do
    let RGB r g b = toSRGB c 
    col <- newZeroRGBA
    setRGBARed col r
    setRGBAGreen col g
    setRGBABlue col b
    pure col 
