{-# LANGUAGE
  TemplateHaskell
#-}
module GUI.Logo
    (
      aurisLogo
    )
where


import           RIO

import           Data.FileEmbed

aurisLogo :: ByteString
aurisLogo = $(embedFile "src/AurisLogo.svg")
