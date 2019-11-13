{-# LANGUAGE
  TemplateHaskell
#-}
module GUI.Logo
  ( aurisLogo
  , aurisLogoBig
  , initLogo
  )
where


import           RIO
import qualified RIO.Text                      as T
import qualified Data.Text.IO                  as T
import           Graphics.UI.FLTK.LowLevel.FLTKHS

import           Data.FileEmbed

import           GUI.Colors


aurisLogo :: ByteString
aurisLogo = $(embedFile "src/AurisLogo.svg")

aurisLogoBig :: ByteString
aurisLogoBig = $(embedFile "src/AurisLogoBig.svg")



initLogo :: Ref Box -> ByteString -> IO ()
initLogo box logoSVG = do
  logo <- svgImageNew logoSVG
  case logo of
    Left err -> do
      T.putStrLn $ "Could not load logo: " <> T.pack (show err)
      exitFailure
    Right svg -> do
      setColor box mcsWhite
      setImage box (Just svg)
