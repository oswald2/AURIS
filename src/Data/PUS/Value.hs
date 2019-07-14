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
module Data.PUS.Value
    (
        Value(..)
        , initialValue
    )
where

import           RIO

--import           Data.Int
--import           Data.Word

import           Data.PUS.Types
import           Data.MIB.Types




data Value =
    ValueInt8 !Int8
    | ValueUInt16 Endian !Word16
    | ValueDouble !Double
    | ValueString !Text
    | ValueFixedString !Word16 !Text
    | ValueUndefined


initialValue :: PTC -> PFC -> Value 
initialValue (PTC 3) (PFC 4) = ValueInt8 0
initialValue _ _ = ValueUndefined