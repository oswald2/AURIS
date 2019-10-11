{-# LANGUAGE OverloadedStrings
    , BangPatterns
    , GeneralizedNewtypeDeriving
    , DeriveGeneric
    , RecordWildCards
    , NoImplicitPrelude
    , BinaryLiterals
    , NumericUnderscores
    , FlexibleInstances
    , GADTs
    , ExistentialQuantification
#-}
module Data.TM.CalibrationTypes
  ( Calibrate(..)
  , CalibInterpolation(..)
  , toCalibInterpolation
  , parseShortTextToDouble
  , parseShortTextToInt64
  , charToRadix
  , charToType
  , Radix(..)
  , NumType(..)
  )
where

import           RIO
import qualified RIO.Text                      as T
import           Data.Text.Short

import           Data.TM.Value
--import           General.Types

import           Text.Megaparsec
import           Text.Megaparsec.Char.Lexer

data Radix =
    Decimal
    | Octal
    | Hex
    deriving (Eq, Ord, Enum, Show, Read)

data NumType =
    NumInteger
    | NumUInteger
    | NumDouble
    deriving (Eq, Ord, Enum, Show, Read)

data CalibInterpolation =
    CalibExtrapolate
    | CalibFail
    deriving (Eq, Ord, Enum, Show, Read)


toCalibInterpolation :: Char -> CalibInterpolation
toCalibInterpolation 'P' = CalibExtrapolate
toCalibInterpolation _   = CalibFail



type Parser = Parsec Void Text


charToRadix :: Char -> Radix
charToRadix 'D' = Decimal
charToRadix 'H' = Hex
charToRadix 'O' = Octal
charToRadix _   = Decimal

charToType :: Char -> NumType
charToType 'I' = NumInteger
charToType 'U' = NumUInteger
charToType 'R' = NumDouble
charToType _   = NumInteger

parseShortTextToDouble :: NumType -> Radix -> ShortText -> Either Text Double
parseShortTextToDouble typ radix x =
  case parseMaybe (doubleParser typ radix) (toText x) of
    Nothing -> Left $ "Could not parse '" <> T.pack (show x) <> "' into Double"
    Just xval -> Right xval


doubleParser :: NumType -> Radix -> Parser Double
doubleParser NumInteger  _       = fromIntegral <$> (decimal :: Parser Int64)
doubleParser NumUInteger Decimal = fromIntegral <$> (decimal :: Parser Word64)
doubleParser NumUInteger Hex = fromIntegral <$> (hexadecimal :: Parser Word64)
doubleParser NumUInteger Octal   = fromIntegral <$> (octal :: Parser Word64)
doubleParser NumDouble   _       = float

parseShortTextToInt64 :: NumType -> Radix -> ShortText -> Either Text Int64
parseShortTextToInt64 typ radix x =
  case parseMaybe (intParser typ radix) (toText x) of
    Nothing -> Left $ "Could not parse '" <> T.pack (show x) <> "' into Int64"
    Just xval -> Right xval


intParser :: NumType -> Radix -> Parser Int64
intParser NumInteger Decimal = decimal
intParser NumInteger Hex     = hexadecimal
intParser NumInteger Octal   = octal
intParser NumUInteger Decimal = decimal
intParser NumUInteger Hex     = hexadecimal
intParser NumUInteger Octal   = octal
intParser NumDouble _ = truncate <$> (float :: Parser Double)


class Calibrate a where
    calibrate :: a -> TMValue -> TMValue

