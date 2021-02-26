{-# LANGUAGE TemplateHaskell 
#-}
module GUI.Theme
  ( setTheme
  , setDarkTheme
  )
where

import RIO

import qualified RIO.ByteString               as B
import qualified RIO.ByteString.Lazy          as BL
import qualified Data.Text.IO as T

import           Codec.Archive.Tar             as T
import           Codec.Compression.GZip

import           Data.FileEmbed

import           System.Directory
import           System.FilePath

import           GI.Gtk as Gtk
import           GI.Gdk

themeFile :: B.ByteString
themeFile = $(makeRelativeToProject "theme.tar.gz" >>= embedFile)


extractTheme :: FilePath -> IO ()
extractTheme homeDir = do 
  T.putStrLn "Extracting Theme..."
  T.unpack (homeDir </> ".themes")
    . T.read
    . decompress
    . BL.fromStrict
    $ themeFile
  T.putStrLn "Done."


checkDirectory :: FilePath -> IO Bool
checkDirectory homeDir = do
  let themeDir = homeDir </> ".themes/Numix-BLACK-SLATE"
  th <- doesDirectoryExist themeDir
  if th
    then do
      T.putStrLn "Theme directory is existing. Not extracting theme."
      return True
    else do
      T.putStrLn "Theme directory does not exist, creating..."
      createDirectoryIfMissing True themeDir
      return False


setDarkTheme :: IO ()
setDarkTheme = do
  homeDir <- getHomeDirectory
  exists  <- checkDirectory homeDir
  unless exists $ do
    extractTheme homeDir

  provider <- cssProviderGetNamed ("Numix-BLACK-SLATE" :: Text) Nothing 
  screenGetDefault >>= \case
    Nothing     -> T.putStrLn "Could not get screen"
    Just screen -> styleContextAddProviderForScreen screen provider 600


setTheme :: IO() 
setTheme = do 
    provider <- Gtk.cssProviderNew
    Gtk.cssProviderLoadFromPath provider "style.css"
    Just screen <- screenGetDefault
    Gtk.styleContextAddProviderForScreen screen provider (fromIntegral Gtk.STYLE_PROVIDER_PRIORITY_USER)

