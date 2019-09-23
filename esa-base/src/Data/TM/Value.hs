{-# LANGUAGE
    OverloadedStrings
    , BangPatterns
    , NoImplicitPrelude
    , DataKinds
    , DeriveGeneric
    , GeneralizedNewtypeDeriving
    , TemplateHaskell
#-}
module Data.TM.Value
  ( TMValueSimple(..)
  , TMValue(..)
  , compareVal
  , isNumeric
  , Data.TM.Value.isValid
  , setValidity
  , tmvalValue
  , tmvalValidity

  )
where

import           RIO

import           Control.Lens                   ( makeLenses )
import           General.Time

import           Data.TM.Validity
import           General.Types



data TMValueSimple =
    TMValInt !Int64
    | TMValUInt !Word64
    | TMValDouble !Double
    | TMValTime !SunTime
    | TMValString !Text
    | TMValOctet !ByteString
    deriving(Show, Generic)


data TMValue = TMValue {
    _tmvalValue :: !TMValueSimple
    , _tmvalValidity :: !Validity
    } deriving (Show, Generic)
makeLenses ''TMValue

instance ToDouble TMValue where
  toDouble TMValue { _tmvalValue = (TMValInt x) }    = fromIntegral x
  toDouble TMValue { _tmvalValue = (TMValUInt x) }   = fromIntegral x
  toDouble TMValue { _tmvalValue = (TMValDouble x) } = x
  toDouble TMValue { _tmvalValue = (TMValTime x) }   = toDouble x
  toDouble TMValue { _tmvalValue = (TMValString _) } = 0
  toDouble TMValue { _tmvalValue = (TMValOctet _) }  = 0



isNumeric :: TMValue -> Bool
isNumeric (TMValue (TMValInt    _) _) = True
isNumeric (TMValue (TMValDouble _) _) = True
isNumeric _                           = False

isValid :: TMValue -> Bool
isValid x = Data.TM.Validity.isValid $ _tmvalValidity x

setValidity :: TMValue -> (Validity -> Validity) -> TMValue
setValidity (TMValue val validity) f = TMValue val (f validity)




-- instance Eq TMIntValue where
--   TMInt  x == TMInt  y = x == y
--   TMUInt x == TMUInt y = x == y
--   TMInt  x == TMUInt y = (x >= 0) && (fromIntegral x == y)
--   TMUInt x == TMInt  y = (y >= 0) && (fromIntegral y == x)


-- instance Ord TMIntValue where
--   compare (TMInt  x) (TMInt  y) = compare x y
--   compare (TMUInt x) (TMUInt y) = compare x y
--   compare (TMInt x) (TMUInt y) =
--     if x < 0 then LT else compare (fromIntegral x) y
--   compare (TMUInt x) (TMInt y) =
--     if y < 0 then GT else compare x (fromIntegral y)


compareVal :: TMValueSimple -> TMValueSimple -> Maybe Ordering
compareVal (TMValInt    x) (TMValInt    y) = Just $ compare x y
compareVal (TMValUInt   x) (TMValUInt   y) = Just $ compare x y
compareVal (TMValDouble x) (TMValDouble y) = Just $ compare x y
compareVal (TMValTime   x) (TMValTime   y) = Just $ compare x y
compareVal (TMValString x) (TMValString y) = Just $ compare x y
compareVal (TMValOctet  x) (TMValOctet  y) = Just $ compare x y

compareVal (TMValInt    x) (TMValDouble y) = Just $ compare (fromIntegral x) y
compareVal (TMValUInt   x) (TMValDouble y) = Just $ compare (fromIntegral x) y
compareVal (TMValDouble x) (TMValInt    y) = Just $ compare x (fromIntegral y)
compareVal (TMValDouble x) (TMValUInt   y) = Just $ compare x (fromIntegral y)

compareVal _               _               = Nothing


instance Eq TMValueSimple where
  val1 == val2 = case compareVal val1 val2 of
    Just EQ -> True
    _       -> False



