{-# LANGUAGE
  TemplateHaskell
#-}
module GUI.About
  ( AboutWindowFluid(..)
  , initAboutWindow
  , aboutWindowShow
  , aboutWindowHide
  , aurisVersion
  )
where

import           RIO
import qualified RIO.Text                      as T
import           Graphics.UI.FLTK.LowLevel.FLTKHS

import           GUI.Logo
import           GUI.Colors

import           Development.GitRev

-- Attribution to the author of the earth SVG icon
-- <div>Icons made by <a href="https://www.flaticon.com/authors/freepik" title="Freepik">Freepik</a> from <a href="https://www.flaticon.com/" title="Flaticon">www.flaticon.com</a></div>

aurisVersion :: Text
aurisVersion = T.concat
  [ "AURISi: AURIS integrated.\n\n"
  , "AURISi Version: 0.1.0.0\n"
  , "Branch: "
  , $(gitBranch)
  , " "
  , $(gitHash)
  , "\ndirty: "
  , dirty
  , "\nCommit Date: "
  , $(gitCommitDate)
  ]

dirty :: Text
dirty | $(gitDirty) = "true"
      | otherwise  = "false"




data AboutWindowFluid = AboutWindowFluid {
  _awWindow :: Ref Window
  , _awLogo :: Ref Box
  , _awText :: Ref TextDisplay
  , _awButtonOK :: Ref Button
  }


initAboutWindow :: AboutWindowFluid -> IO ()
initAboutWindow a@AboutWindowFluid {..} = do
  mcsWindowSetColor _awWindow
  mcsTextDisplaySetColor _awText
  mcsButtonSetColor _awButtonOK
  initLogo _awLogo aurisLogoBig

  buf <- textBufferNew Nothing Nothing
  setBuffer _awText (Just buf)

  setText buf aurisVersion

  setCallback _awButtonOK (okCB a)



aboutWindowShow :: AboutWindowFluid -> IO ()
aboutWindowShow AboutWindowFluid {..} = showWidget _awWindow

aboutWindowHide :: AboutWindowFluid -> IO ()
aboutWindowHide AboutWindowFluid {..} = hide _awWindow


okCB :: AboutWindowFluid -> Ref Button -> IO ()
okCB AboutWindowFluid {..} _ = do
  hide _awWindow
  return ()
