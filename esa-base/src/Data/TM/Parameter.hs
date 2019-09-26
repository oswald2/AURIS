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




data TMParameter = TMParameter {
    _pName :: !ShortText
    , _pTime :: !SunTime
    , _pValue :: TMValue
    , _pEngValue :: Maybe TMValue
} deriving (Show)

