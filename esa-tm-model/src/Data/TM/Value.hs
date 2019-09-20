{-# LANGUAGE
    OverloadedStrings
    , BangPatterns
    , NoImplicitPrelude
    , DataKinds
    , DeriveGeneric
#-}
module Data.TM.Value
  ( TMValueSimple(..)
  , TMValue(..)
  )
where

import           RIO
import qualified RIO.Text                      as T

import           Control.Exception

import           General.Time

import           Data.TM.Validity



data TMValueSimple =
    TMValInt Int64
    | TMValUInt Word64
    | TMValDouble Double
    | TMValTime SunTime
    | TMValString Text
    | TMValOctet ByteString
    deriving(Eq, Show, Generic)


data TMValue = TMValue {
    _tmvalValue :: TMValueSimple
    , _tmvalValidity :: Validity
    } deriving (Eq, Show, Generic)



data TMValueException = TMValueException Text
    deriving Show

instance Exception TMValueException

instance Ord TMValueSimple where
  compare (TMValInt    x) (TMValInt    y) = compare x y
  compare (TMValUInt   x) (TMValUInt   y) = compare x y
  compare (TMValDouble x) (TMValDouble y) = compare x y
  compare (TMValTime   x) (TMValTime   y) = compare x y
  compare (TMValString x) (TMValString y) = compare x y
  compare (TMValOctet  x) (TMValOctet  y) = compare x y

  compare (TMValInt x) (TMValUInt y) =
    if x < 0 then LT else compare (fromIntegral x) y
  compare (TMValUInt x) (TMValInt y) =
    if y < 0 then GT else compare x (fromIntegral y)
  compare (TMValInt    x) (TMValDouble y) = compare (fromIntegral x) y
  compare (TMValDouble x) (TMValInt    y) = compare x (fromIntegral y)

  compare (TMValUInt   x) (TMValDouble y) = compare (fromIntegral x) y
  compare (TMValDouble x) (TMValUInt   y) = compare x (fromIntegral y)

  compare x               y               = throw
    (TMValueException
      ("Cannot compare " <> T.pack (show x) <> " with " <> T.pack (show y))
    )






