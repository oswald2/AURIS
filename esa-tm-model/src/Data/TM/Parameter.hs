{-# LANGUAGE
    OverloadedStrings
    , BangPatterns
    , NoImplicitPrelude
#-}
module Data.TM.Parameter
  ( TMParameter(..)
  )
where

import           RIO

import           General.Time
import           Data.TM.Value
import           Data.TM.Validity



data TMParameter = TMParameter {
    _pName :: !Text
    , _pTime :: !SunTime
    , _pValue :: TMValueSimple
    , _pEngValue :: TMValueSimple
    , _pValidity :: Validity
} deriving (Show)

