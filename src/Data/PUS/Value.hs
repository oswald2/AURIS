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
    )
where

import           RIO

--import           Data.Int
--import           Data.Word

import           Data.PUS.Types




data Value =
    ValueInt8 !Int8
    | ValueUInt16 Endian !Word16
