module General.TextTools
    ( escapeText
    , escapeTextBuilder
    ) where

import           RIO
import qualified RIO.Text                      as T

import           Text.Builder                  as TB




escapeText :: Text -> Text 
escapeText txt = TB.run $ escapeTextBuilder txt

escapeTextBuilder :: Text -> TB.Builder
escapeTextBuilder txt = mconcat . map convChar . filter isLegal . T.unpack $ txt
  where
    isLegal c =
        c
            == '\t'
            || c
            == '\n'
            || c
            == '\r'
            || (c >= '\x20' && c <= '\xD7FF')
            || (c >= '\xE000' && c <= '\xFFFD')
            || (c >= '\x10000' && c <= '\x10FFFF')



convChar :: Char -> TB.Builder
convChar x = case x of
    '&' -> text "&amp;"
    '<' -> text "&lt;"
    '>' -> text "&gt;"
    '"' -> text "&quot;"
    c   -> char c
