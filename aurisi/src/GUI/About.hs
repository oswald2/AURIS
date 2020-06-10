{-# LANGUAGE OverloadedStrings
    , TemplateHaskell 
    , QuasiQuotes
    , ForeignFunctionInterface     
#-}
module GUI.About
  ( createAboutDialog
  )
where


import           GI.Gtk                        as Gtk

import           Data.Text                     as T

import           Version

import           Data.FileEmbed
import           GUI.Logo


license :: Text
license = T.pack $(makeRelativeToProject "LICENSE" >>= embedStringFile)


-- foreign import ccall unsafe "my_pixbuf" pixbufLogo :: Ptr CUChar



createAboutDialog :: IO Gtk.AboutDialog
createAboutDialog = do
    -- setup about dialog
  aboutDialog <- aboutDialogNew

  --logo        <- pixbufNewFromInline pixbufLogo
  --logo        <- getLogoPixbuf

  set
    aboutDialog
    [ aboutDialogProgramName := ("AURISi" :: Text)
    , aboutDialogVersion := aurisVersion
    , aboutDialogCopyright := ("(C) by Michael Oswald" :: Text)
    , aboutDialogComments
      := ("AURIS: an open source mission control system.\n\n" :: Text)
    , aboutDialogAuthors
      := ["Michael Oswald (michael.oswald@onikudaki.net)" :: Text]
    , aboutDialogLicense := license
    , aboutDialogWrapLicense := True
    --, aboutDialogLogo := Just logo
    ]

  return aboutDialog


