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
  case parseMaybe (numParser typ radix) (toText x) of
    Nothing -> Left $ "Could not parse '" <> T.pack (show x) <> "' into double"
    Just xval -> Right xval


numParser :: NumType -> Radix -> Parser Double
numParser NumInteger  _       = fromIntegral <$> (decimal :: Parser Int64)
numParser NumUInteger Decimal = fromIntegral <$> (decimal :: Parser Word64)
numParser NumUInteger Hex = fromIntegral <$> (hexadecimal :: Parser Word64) 
numParser NumUInteger Octal = fromIntegral <$> (octal :: Parser Word64)
numParser NumDouble _ = float


class Calibrate a where
    calibrate :: a -> TMValue -> TMValue

