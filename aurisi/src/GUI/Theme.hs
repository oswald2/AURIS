{-# LANGUAGE TemplateHaskell 
#-}
module GUI.Theme
  ( setTheme
  )
where

import RIO

import qualified Data.ByteString               as B
import qualified Data.ByteString.Lazy          as BL
import           Data.Text                      ( Text )
import           Control.Monad

import           Codec.Archive.Tar             as T
import           Codec.Compression.GZip

import           Data.FileEmbed

import           System.Directory
import           System.FilePath

import           Utils.Logger

import           Graphics.UI.Gtk.General.CssProvider
import           Graphics.UI.Gtk.General.StyleContext
import           Graphics.UI.Gtk.Gdk.Screen


themeFile :: B.ByteString
themeFile = $(makeRelativeToProject "theme.tar.gz" >>= embedFile)


extractTheme :: FilePath -> IO ()
extractTheme homeDir = do 
  logStr lAreaAlways "Extracting Theme..."
  T.unpack (homeDir </> ".themes")
    . T.read
    . decompress
    . BL.fromStrict
    $ themeFile
  logStr lAreaAlways "Done."


checkDirectory :: FilePath -> IO Bool
checkDirectory homeDir = do
  let themeDir = homeDir </> ".themes/Numix-BLACK-SLATE"
  th <- doesDirectoryExist themeDir
  if th
    then do
      logStr lAreaAlways "Theme directory is existing. Not extracting theme."
      return True
    else do
      logStr lAreaAlways "Theme directory does not exist, creating..."
      createDirectoryIfMissing True themeDir
      return False


setTheme :: IO ()
setTheme = do
  homeDir <- getHomeDirectory
  exists  <- checkDirectory homeDir
  unless exists $ do
    extractTheme homeDir

  cssProviderGetNamed ("Numix-BLACK-SLATE" :: Text) Nothing >>= \case
    Nothing       -> logStr lAreaAlways "Could not load GTK theme"
    Just provider -> do
      screenGetDefault >>= \case
        Nothing     -> logStr lAreaAlways "Could not get screen"
        Just screen -> styleContextAddProviderForScreen screen provider 600

