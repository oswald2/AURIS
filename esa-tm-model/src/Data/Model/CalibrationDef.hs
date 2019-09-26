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
module Data.Model.CalibrationDef
   (
    CalibrationDef(..)
   )
where


import RIO 





data CalibrationDef = CalibrationDef
    deriving (Show, Generic)