{-# LANGUAGE
    OverloadedStrings
    , BangPatterns
    , NoImplicitPrelude
    , GADTs
#-}
module Data.TM.Parameter
  ( TMParameter(..)
  , TMSumParameter(..)
  , Validity
  , getRawValidity
  , mkValidity
  , ValidityVal(..)
  )
where

import           RIO

import           Data.Bits

import           General.Time

import           Data.TM.Value


newtype Validity = Validity { getRawValidity :: Word32 }
    deriving (Eq, Show, Read)

data ValidityVal = ValidityOK | ValidityOOL | ValidityDivisionByZero
    deriving (Eq, Ord, Enum)

mkValidity :: [ValidityVal] -> Validity
mkValidity = Validity . foldl' (\acc x -> acc .|. valToBit x) 0

valToBit :: ValidityVal -> Word32
valToBit ValidityOK             = 0x00000001
valToBit ValidityOOL            = 0x00000002
valToBit ValidityDivisionByZero = 0x00000004


data TMParameter a b = TMParameter {
    _pName :: !Text
    , _pTime :: !SunTime
    , _pValue :: a
    , _pEngValue :: b
    , _pValidity :: Validity
} deriving (Show)


data TMSumParameter where
    TMSUU ::TMParameter TMValUInt TMValUInt -> TMSumParameter
    TMSUT ::TMParameter TMValUInt TMValString -> TMSumParameter
    TMSDD ::TMParameter TMValDouble TMValDouble -> TMSumParameter
    TMSII ::TMParameter TMValInt TMValInt -> TMSumParameter
