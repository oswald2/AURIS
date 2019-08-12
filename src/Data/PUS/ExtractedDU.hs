{-# LANGUAGE
    OverloadedStrings
    , BangPatterns
    , NoImplicitPrelude
    , TemplateHaskell
#-}
module Data.PUS.ExtractedDU
    ( ExtractedDU(..)
    , epDU
    , epQuality
    , epSource
    , epGap
    )
where


import           RIO

import           Control.Lens                   ( makeLenses )

import           Data.PUS.Types

import           Protocol.ProtocolInterfaces


data ExtractedDU a = ExtractedDU {
    _epQuality :: Flag Good
    , _epGap ::Maybe (Word32, Word32)
    , _epSource :: !ProtocolInterface
    , _epDU :: a
}
makeLenses ''ExtractedDU


