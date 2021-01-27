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

import           RIO                     hiding ( (.~) )

import           Control.Lens                   ( (.~) )

import           Data.Text.Short               as T

import           General.Types
import           General.Time

import           Data.PUS.Parameter
import           Data.PUS.Value
import           Data.PUS.EncTime

import           Data.TM.Parameter
import           Data.TM.Value
import           Data.TM.Validity



extParamToParVal :: SunTime -> Epoch -> Validity -> ExtParameter -> TMParameter
extParamToParVal timestamp epoch validity ExtParameter {..} = TMParameter
  { _pName     = T.fromText _extParName
  , _pTime     = timestamp
  , _pValue    = valueToTMValue epoch validity _extParValue
  , _pEngValue = Nothing
  }

valueToTMValue :: Epoch -> Validity -> Value -> TMValue
valueToTMValue _ validity (ValUInt8X _ x) =
  TMValue (TMValUInt (fromIntegral x)) validity
valueToTMValue _ validity (ValUInt8 x) =
  TMValue (TMValUInt (fromIntegral x)) validity
valueToTMValue _ validity (ValUInt16X _ x) =
  TMValue (TMValUInt (fromIntegral x)) validity
valueToTMValue _ validity (ValUInt16 _ x) =
  TMValue (TMValUInt (fromIntegral x)) validity
valueToTMValue _ validity (ValUInt32X _ x) =
  TMValue (TMValUInt (fromIntegral x)) validity
valueToTMValue _ validity (ValUInt24 _ x) =
  TMValue (TMValUInt (fromIntegral x)) validity
valueToTMValue _ validity (ValUInt32 _ x) =
  TMValue (TMValUInt (fromIntegral x)) validity
valueToTMValue _ validity (ValUInt64 _ x) =
  TMValue (TMValUInt x) validity
valueToTMValue _ validity (ValInt8 x) =
  TMValue (TMValInt (fromIntegral x)) validity
valueToTMValue _ validity (ValInt16 _ x) =
  TMValue (TMValInt (fromIntegral x)) validity
valueToTMValue _ validity (ValInt24 _ x) =
  TMValue (TMValInt (fromIntegral x)) validity
valueToTMValue _ validity (ValInt32 _ x) =
  TMValue (TMValInt (fromIntegral x)) validity
valueToTMValue _ validity (ValInt64 _ x) =
  TMValue (TMValInt x) validity
valueToTMValue _ validity (ValDouble _ x) = TMValue (TMValDouble x) validity
valueToTMValue _ validity (ValString x       ) = checkString validity x
valueToTMValue _ validity (ValFixedString _ x) = checkString validity x
valueToTMValue _ validity (ValOctet x        ) = TMValue (TMValOctet x) validity
valueToTMValue _ validity (ValFixedOctet _ x ) = TMValue (TMValOctet x) validity
valueToTMValue epoch validity (ValCUCTime x) =
  let t = cucTimeToSunTime epoch x in TMValue (TMValTime t) validity
valueToTMValue _ validity ValUndefined =
  TMValue (TMValUInt 0) (setUndefinedValue validity)



checkString :: Validity -> ByteString -> TMValue
checkString validity x = case T.fromByteString x of
  Just s  -> TMValue (TMValString s) validity
  Nothing -> TMValue (TMValString T.empty) (setStringNotUtf8 validity)


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

