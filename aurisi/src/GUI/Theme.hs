{-# LANGUAGE TemplateHaskell 
#-}
module GUI.Theme
  ( setTheme
  )
where

import RIO

import qualified RIO.ByteString               as B
import qualified RIO.ByteString.Lazy          as BL
import           Data.Text                      ( Text )
import           Control.Monad

import           Codec.Archive.Tar             as T
import           Codec.Compression.GZip

import           Data.FileEmbed

import           System.Directory
import           System.FilePath

-- import           Graphics.UI.Gtk.General.CssProvider
-- import           Graphics.UI.Gtk.General.StyleContext
-- import           Graphics.UI.Gtk.Gdk.Screen

import GI.Gtk as Gtk
import GI.Gdk

themeFile :: B.ByteString
themeFile = $(makeRelativeToProject "theme.tar.gz" >>= embedFile)


extractTheme :: FilePath -> IO ()
extractTheme homeDir = do 
  putStrLn "Extracting Theme..."
  T.unpack (homeDir </> ".themes")
    . T.read
    . decompress
    . BL.fromStrict
    $ themeFile
  putStrLn "Done."


checkDirectory :: FilePath -> IO Bool
checkDirectory homeDir = do
  let themeDir = homeDir </> ".themes/Numix-BLACK-SLATE"
  th <- doesDirectoryExist themeDir
  if th
    then do
      putStrLn "Theme directory is existing. Not extracting theme."
      return True
    else do
      putStrLn "Theme directory does not exist, creating..."
      createDirectoryIfMissing True themeDir
      return False


setTheme :: IO ()
setTheme = do
  homeDir <- getHomeDirectory
  exists  <- checkDirectory homeDir
  unless exists $ do
    extractTheme homeDir

  provider <- cssProviderGetNamed ("Numix-BLACK-SLATE" :: Text) Nothing 
  screenGetDefault >>= \case
    Nothing     -> putStrLn "Could not get screen"
    Just screen -> styleContextAddProviderForScreen screen provider 600

