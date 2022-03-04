module Data.Conversion.Types
    ( Warnings
    , getVal
    , getValFromTemplate
    ) where

import           RIO
import qualified RIO.Text                      as T

import           Data.Text.Short                ( ShortText )
import qualified Data.Text.Short               as ST

import           Data.TM.Value

import           General.Time
import           General.Types                  ( parseHexLine )

import           Data.Attoparsec.Text


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


getValFromTemplate
    :: TMValueSimple -> Radix -> ShortText -> Either Text TMValueSimple
getValFromTemplate (TMValInt _) radix str =
    TMValInt <$> parseShortTextToInt64 NumInteger radix str
getValFromTemplate (TMValUInt _) radix str =
    TMValUInt <$> parseShortTextToWord64 NumInteger radix str
getValFromTemplate (TMValDouble _) radix str =
    TMValDouble <$> parseShortTextToDouble NumInteger radix str
getValFromTemplate (TMValTime _) _radix str =
    TMValTime <$> General.Time.fromText (ST.toText str)
getValFromTemplate (TMValString _) _radix str = Right $ TMValString str
getValFromTemplate (TMValOctet _) _radix str =
    case parseOnly parseHexLine (ST.toText str) of
        Left err ->
            Left $ "Could not parse value '" <> ST.toText str <> "' as hex value: " <> T.pack err
        Right val -> Right (TMValOctet val)
getValFromTemplate TMValNothing _ _ = Right TMValNothing


