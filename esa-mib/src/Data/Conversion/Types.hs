module Data.Conversion.Types
    ( Warnings
    , getVal
    ) where

import           RIO
import qualified RIO.Text                      as T
import           Data.Text.Short                ( ShortText )
import qualified Data.Text.Short               as ST
import           Data.TM.Value
import           General.Time

type Warnings = Maybe Text


getVal :: Char -> Radix -> ShortText -> Either Text TMValueSimple
getVal format radix str = case format of
    'A' -> Right $ TMValString str
    'I' -> TMValInt <$> parseShortTextToInt64 NumInteger radix str
    'U' -> TMValUInt <$> parseShortTextToWord64 NumInteger radix str
    'R' -> TMValDouble <$> parseShortTextToDouble NumInteger radix str
    'T' -> TMValTime <$> General.Time.fromText (ST.toText str)
    'D' -> TMValTime <$> General.Time.fromText (ST.toText str)
    x   -> Left $ "Illegal type specified for value field: " <> T.singleton x
