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
    )
where

import           RIO

import           Control.Lens                   ( (.~) )

import           Data.Text.Short               as T

import           General.Types
import           General.Time

import           Data.PUS.Parameter
import           Data.PUS.Value

import           Data.TM.Parameter
import           Data.TM.Value
import           Data.TM.Validity



extParamToParVal :: SunTime -> Validity -> ExtParameter -> TMParameter
extParamToParVal timestamp validity ExtParameter {..} = TMParameter
    { _pName     = T.fromText _extParName
    , _pTime     = timestamp
    , _pValue    = valueToTMValue validity _extParValue
    , _pEngValue = Nothing
    }

valueToTMValue :: Validity -> Value -> TMValue
valueToTMValue validity (ValUInt3 x) = TMValue (TMValUInt (fromIntegral x)) validity
valueToTMValue validity (ValInt8 x) = TMValue (TMValInt (fromIntegral x)) validity
valueToTMValue validity (ValInt16 _ x) = TMValue (TMValInt (fromIntegral x)) validity
valueToTMValue validity (ValDouble _ x) = TMValue (TMValDouble x) validity
valueToTMValue validity (ValString x) = checkString validity x
valueToTMValue validity (ValFixedString _ x) = checkString validity x
valueToTMValue validity (ValOctet x) = TMValue (TMValOctet x) validity
valueToTMValue validity (ValFixedOctet _ x) = TMValue (TMValOctet x) validity
-- TODO: convert cuc time to sun time
valueToTMValue validity (ValCUCTime _x) = TMValue (TMValTime nullTime) validity
valueToTMValue validity ValUndefined = TMValue (TMValUInt 0) (setUndefinedValue validity)



checkString :: Validity -> ByteString -> TMValue
checkString validity x =
    case T.fromByteString x of
        Just s -> TMValue (TMValString s) validity
        Nothing -> TMValue (TMValString (T.empty)) (setStringNotUtf8 validity)


extractExtParameters :: ByteString -> [ExtParameter] -> [ExtParameter]
extractExtParameters bytes = map (getExtParameter' bytes)


getExtParameter' :: ByteString -> ExtParameter -> ExtParameter
getExtParameter' bytes param =
    let (bo, BitOffset bits) = offsetParts off
        bitOffset            = param ^. extParOff
        off                  = toOffset bitOffset
        value                = param ^. extParValue
    in  if bits == 0 && isSetableAligned value
            then param & extParValue .~ getAlignedValue bytes bo value
            else if isGettableUnaligned value
                then param & extParValue .~ getUnalignedValue bytes off value
                else
                         -- in this case, we go to the next byte offset. According
                         -- to PUS, we cannot set certain values on non-byte boundaries
                    let newOffset = nextByteAligned bitOffset
                        newParam  = param & extParOff .~ newOffset
                    in  getExtParameter' bytes newParam

