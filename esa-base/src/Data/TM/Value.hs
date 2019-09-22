{-# LANGUAGE
    OverloadedStrings
    , BangPatterns
    , NoImplicitPrelude
    , DataKinds
    , DeriveGeneric
    , GeneralizedNewtypeDeriving
#-}
module Data.TM.Value
  ( TMValueSimple(..)
  , TMValue(..)
  , compareVal
  )
where

import           RIO
--import qualified RIO.Text                      as T

--import           Control.Exception

import           General.Time

import           Data.TM.Validity



data TMIntValue = TMInt Int64 | TMUInt Word64
    deriving (Show, Generic)

newtype TMDoubleValue = TMDouble Double
    deriving (Eq, Ord, Num, Fractional, Floating, Real, Show, Generic)


data TMValueSimple =
    TMValInt TMIntValue
    | TMValDouble TMDoubleValue
    | TMValTime SunTime
    | TMValString Text
    | TMValOctet ByteString
    deriving(Show, Generic)


data TMValue = TMValue {
    _tmvalValue :: TMValueSimple
    , _tmvalValidity :: Validity
    } deriving (Show, Generic)



newtype TMValueException = TMValueException Text
    deriving Show

instance Exception TMValueException



instance Eq TMIntValue where
  TMInt  x == TMInt  y = x == y
  TMUInt x == TMUInt y = x == y
  TMInt  x == TMUInt y = (x >= 0) && (fromIntegral x == y)
  TMUInt x == TMInt  y = (y >= 0) && (fromIntegral y == x)


instance Ord TMIntValue where
  compare (TMInt  x) (TMInt  y) = compare x y
  compare (TMUInt x) (TMUInt y) = compare x y
  compare (TMInt x) (TMUInt y) =
    if x < 0 then LT else compare (fromIntegral x) y
  compare (TMUInt x) (TMInt y) =
    if y < 0 then GT else compare x (fromIntegral y)

instance Num TMIntValue where
    (TMInt x) + (TMInt y) = TMInt $ x + y
    (TMUInt x) + (TMUInt y) = TMUInt $ x + y

 
compareVal :: TMValueSimple -> TMValueSimple -> Maybe Ordering
compareVal (TMValInt x) (TMValInt y) = Just $ compare x y
compareVal (TMValDouble x) (TMValDouble y) = Just $ compare x y
compareVal (TMValTime x) (TMValTime y) = Just $ compare x y
compareVal (TMValString x) (TMValString y) = Just $ compare x y
compareVal (TMValOctet x) (TMValOctet y) = Just $ compare x y

compareVal (TMValInt (TMInt x)) (TMValDouble y) = Just $ compare (fromIntegral x) y
compareVal (TMValInt (TMUInt x)) (TMValDouble y) = Just $ compare (fromIntegral x) y
compareVal (TMValDouble x) (TMValInt (TMInt y)) = Just $ compare x (fromIntegral y)
compareVal (TMValDouble x) (TMValInt (TMUInt y)) = Just $ compare x (fromIntegral y)

compareVal _ _ = Nothing


instance Eq TMValueSimple where 
    val1 == val2 = case compareVal val1 val2 of
        Just EQ -> True
        _ -> False            



