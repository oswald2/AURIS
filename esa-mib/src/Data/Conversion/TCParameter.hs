module Data.Conversion.TCParameter
    ( convertParam
    ) where

import           RIO
import qualified Data.Text.Short               as ST

import           Data.MIB.CPC

import           Data.PUS.Parameter
import           Data.PUS.Value

import           General.PUSTypes

-- import           Text.Megaparsec
-- import           Text.Megaparsec.Char
-- import qualified Text.Megaparsec.Char.Lexer    as L

import           Numeric


convertParam :: CPCentry -> Parameter
convertParam e@CPCentry {..} = Parameter { _paramName  = ST.toText _cpcName
                                         , _paramValue = determineValue e
                                         }



determineValue :: CPCentry -> Value
determineValue CPCentry {..} = initialValue BiE (PTC _cpcPTC) (PFC _cpcPFC)
    -- TODO: set default value, parse from _cpcDefVal



-- parseShortTextToValue
--   :: PTC -> PFC -> ShortText -> Either Text TMValueSimple
-- parseShortTextToValue ptc pfc x =
--   case parseMaybe (valueParser ptc pfc) (toText x) of
--     Nothing   -> Left $ "Could not parse '" <> toText x <> "' into PTC " <> textDisplay ptc <> " PFC " <> text Display pfc
--     Just xval -> Right xval



-- double :: Parser Double
-- double = L.signed space L.float

-- integer :: Parser Int64
-- integer = L.lexeme space L.decimal

-- uInteger :: Parser Word64
-- uInteger = L.lexeme space L.decimal

-- signedInteger :: Parser Int64
-- signedInteger = L.signed space integer

-- hexInteger :: Parser Word64
-- hexInteger = L.lexeme space L.hexadecimal

-- octInteger :: Parser Word64
-- octInteger = L.lexeme space L.octal


-- {-# INLINABLE tmValueParser #-}
-- valueParser :: PTC -> PFC -> Parser Value
-- valueParser (PTC ptc) (PFC pfc)
--   | ptc == 1 || ptc == 2 || ptc == 3
--   = TMValUInt <$> uInteger
--   | ptc == 4
--   = TMValInt <$> signedInteger
--   | ptc == 5
--   = TMValDouble <$> double
--   | ptc == 6 && pfc > 0
--   = TMValUInt <$> uInteger
--   | ptc == 7 && pfc == 0
--   = TMValOctet . strToByteString <$> many hexDigitChar
--   | ptc == 7
--   = TMValOctet . strToByteString <$> count (2 * pfc) hexDigitChar
--   | ptc == 8 && pfc == 0
--   = TMValString . Data.Text.Short.fromText <$> takeRest
--   | ptc == 8
--   = TMValString . Data.Text.Short.fromText <$> takeP Nothing pfc
--   | ptc == 9 || ptc == 10
--   = TMValTime <$> sunTimeParser
--   | ptc == 11 || ptc == 13
--   = pure nullValueSimple
--   | otherwise
--   = fancyFailure
--     .  S.singleton
--     .  ErrorFail
--     $  "Illegal type for TMValue (PTC="
--     <> show ptc
--     <> ", PFC="
--     <> show pfc
--     <> ")"
