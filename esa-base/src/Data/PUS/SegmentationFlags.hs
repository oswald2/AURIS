{-# LANGUAGE OverloadedStrings
    , BangPatterns
    , GeneralizedNewtypeDeriving
    , DeriveGeneric
    , RecordWildCards
    , NoImplicitPrelude
#-}
module Data.PUS.SegmentationFlags
    ( SegmentationFlags(..)
    )
where

import           RIO

import           Data.Binary
import           Data.Aeson
import           Codec.Serialise


data SegmentationFlags = SegmentFirst
    | SegmentContinue
    | SegmentLast
    | SegmentStandalone
    deriving (Ord, Eq, Enum, Show, Read, Generic)

instance Binary SegmentationFlags
instance NFData SegmentationFlags
instance Serialise SegmentationFlags
instance NFData SegmentationFlags
instance FromJSON SegmentationFlags
instance ToJSON SegmentationFlags where
    toEncoding = genericToEncoding defaultOptions

instance Display SegmentationFlags where 
    display SegmentFirst = "FIRST"
    display SegmentContinue = "CONTINUE"
    display SegmentLast = "LAST"
    display SegmentStandalone = "UNSEGMENTED"
