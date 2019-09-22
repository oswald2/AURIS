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
import           Data.Text.Short

import           General.Time
import           Data.TM.Value
import           Data.TM.Validity



data TMParameter = TMParameter {
    _pName :: !ShortText
    , _pTime :: !SunTime
    , _pValue :: TMValueSimple
    , _pEngValue :: TMValueSimple
    , _pValidity :: Validity
} deriving (Show)

