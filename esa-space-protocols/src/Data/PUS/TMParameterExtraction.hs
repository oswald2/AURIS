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
module Data.PUS.TMParameterExtraction
    ( extractExtParameters
    , extParamToParVal
    ) where

import           RIO                     hiding ( (.~) )

import           Control.Lens                   ( (.~) )

import           Data.Text.Short               as T

import           General.Types
import           General.Time

import           Data.PUS.Parameter
import           Data.PUS.Value

import           Data.TM.Parameter
import           Data.TM.Validity
import           Data.TC.ValueConversion



extParamToParVal :: SunTime -> Epoch -> Validity -> ExtParameter -> TMParameter
extParamToParVal timestamp epoch validity ExtParameter {..} = TMParameter
    { _pName     = T.fromText _extParName
    , _pTime     = timestamp
    , _pValue    = valueToTMValue epoch validity _extParValue
    , _pEngValue = Nothing
    }


extractExtParameters :: ByteString -> [ExtParameter] -> Maybe [Parameter]
extractExtParameters bytes = mapM proc
    where proc p = extParamToParam <$> getExtParameter' bytes p



getExtParameter' :: ByteString -> ExtParameter -> Maybe ExtParameter
getExtParameter' bytes param =
    let (bo, BitOffset bits) = offsetParts off
        bitOffset            = param ^. extParOff
        off                  = toOffset bitOffset
        value                = param ^. extParValue
    in  if bits == 0 && isSetableAligned value
            then case getAlignedValue bytes bo value of
                Just v  -> Just $ param & extParValue .~ v
                Nothing -> Nothing
            else if isGettableUnaligned value
                then case getUnalignedValue bytes off value of
                    Just v  -> Just $ param & extParValue .~ v
                    Nothing -> Nothing
                else
                         -- in this case, we go to the next byte offset. According
                         -- to PUS, we cannot set certain values on non-byte boundaries
                    let newOffset = nextByteAligned bitOffset
                        newParam  = param & extParOff .~ newOffset
                    in  getExtParameter' bytes newParam

