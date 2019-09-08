{-# LANGUAGE
    OverloadedStrings
    , BangPatterns
    , NoImplicitPrelude
#-}
module Data.TM.Parameter
(
    TMParameter(..)
)
where

import RIO

import General.Time


newtype Validity = Validity { getRawValidity :: Word32 }
    deriving (Eq, Show, Read)


data TMParameter a b = TMParameter {
    _pName :: !Text
    , _pTime :: !SunTime
    , _pValue :: a
    , _pEngValue :: b
    , _pValidity :: Validity
} deriving (Show)

