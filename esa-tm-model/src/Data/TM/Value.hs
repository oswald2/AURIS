{-# LANGUAGE
    OverloadedStrings
    , BangPatterns
    , NoImplicitPrelude
    , DataKinds
    , GeneralizedNewtypeDeriving
#-}
module Data.TM.Value
(
    ValType(..)
    , TMValInt(..)
    , TMValUInt(..)
    , TMValDouble(..)
    , TMValTime(..)
    , TMValString(..)
    , TMSumValue(..)
)
where

import RIO

import General.Time

data ValType = ValInt | ValUInt | ValDouble | ValTime | ValString


newtype TMValInt = TMValInt Int64
    deriving (Show, Eq, Ord, Enum, Num, Real, Integral)

newtype TMValUInt = TMValUInt Word64
    deriving (Show, Eq, Ord, Enum, Num, Real, Integral)

newtype TMValDouble = TMValDouble Double
    deriving (Show, Eq, Ord, Num, Real, Fractional, Floating, RealFrac, RealFloat)

newtype TMValTime = TMValTime SunTime
    deriving (Show, Eq, Ord)

newtype TMValString = TMValString Text
    deriving (Show, Eq, Semigroup, Monoid)


    

data TMSumValue = 
    TMSValInt TMValInt
    | TMSValUint TMValUInt
    | TMSValDouble TMValDouble
    | TMSValTime TMValTime
    | TMSValString TMValString
    deriving (Show)

