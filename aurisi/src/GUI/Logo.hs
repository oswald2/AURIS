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
import qualified RIO.ByteString                as B
import           Graphics.UI.FLTK.LowLevel.FLTKHS

import           Data.FileEmbed

import           GUI.Colors


aurisLogo :: ByteString
aurisLogo = $(makeRelativeToProject "src/AurisLogo.svg" >>= embedFile)
--aurisLogo = B.empty

aurisLogoBig :: ByteString
aurisLogoBig = $(makeRelativeToProject "src/AurisLogoBig.svg" >>= embedFile)
--aurisLogoBig = B.empty


initLogo :: Ref Box -> ByteString -> IO ()
initLogo box logoSVG = do
  logo <- svgImageNew logoSVG
  case logo of
    Left err -> do
      T.putStrLn $ "Could not load logo: " <> T.pack (show err)
      exitFailure
    Right svg -> do
      setColor box mcsWhite
      (Rectangle _ s1@(Size (Width w) (Height h))) <- getRectangle box 
      s2@(Size (Width iw) (Height ih)) <- getDataSize svg 
      let size = Size (Width (w * 2)) (Height (h * 2))
      print (s1, s2)
      scale svg size (Just True) Nothing
      setImage box (Just svg)
