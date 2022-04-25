module Data.TC.RangeSet
    ( RangeSet(..)
    , RangeValue(..)
    , rangeSetCheck
    , rangeSetBuilder
    ) where

import           RIO
--import qualified RIO.Text                      as T
import qualified RIO.Vector                    as V

import           Data.Text.Short                ( ShortText )
import qualified Data.Text.Short               as ST


import           Codec.Serialise
import           Data.Aeson

import           Data.TM.Value
import           General.Types

import           Text.Builder                  as TB


data RangeValue =
  RangeDiscrete !TMValueSimple
  | RangeMinMax !TMValueSimple !TMValueSimple
    deriving(Show, Generic)

instance Serialise RangeValue
instance FromJSON RangeValue
instance ToJSON RangeValue where
    toEncoding = genericToEncoding defaultOptions

rangeValueBuilder :: RangeValue -> TB.Builder
rangeValueBuilder (RangeDiscrete v) = text (textDisplay v)
rangeValueBuilder (RangeMinMax v1 v2) =
    char '('
        <> text (textDisplay v1)
        <> text ", "
        <> text (textDisplay v2)
        <> char ')'


{-# INLINABLE checkRange #-}
checkRange :: TMValueSimple -> RangeValue -> Bool
checkRange val (RangeDiscrete v) = v == val
checkRange val (RangeMinMax vl vh) =
    let lo' = compareVal vl val
        hi' = compareVal val vh
    in  case (lo', hi') of
            (Just lo, Just hi) ->
                ((lo == LT) || (lo == EQ)) && ((hi == LT) || (hi == EQ))
            _ -> False


data RangeSet = RangeSet
    { _rsIdent  :: !ShortText
    , _rsDescr  :: !ShortText
    , _rsInter  :: !ValInter
    , _rsValues :: Vector RangeValue
    }
    deriving (Show, Generic)

instance Serialise RangeSet
instance FromJSON RangeSet
instance ToJSON RangeSet where
    toEncoding = genericToEncoding defaultOptions

rangeSetBuilder :: Word16 -> RangeSet -> TB.Builder 
rangeSetBuilder indent range = 
    indentBuilder indent <> padRight 23 (text "<b>Name:</b> ") <> text (ST.toText (_rsIdent range))
    <> newLineIndentBuilder indent (padRight 23 (text "<b>Description:</b> ")) <> text (ST.toText (_rsDescr range))
    <> newLineIndentBuilder indent (padRight 23 (text "<b>Representation:</b> ")) <> text (textDisplay (_rsInter range))
    <> newLineIndentBuilder indent (text "<b>Values:</b>\n")
    <> newLineIndentBuilder indent (values (indent + 4))
    where 
        values ind = 
            TB.intercalate (char '\n' <> indentBuilder ind) . map rangeValueBuilder . V.toList . _rsValues $ range


{-# INLINABLE rangeSetCheck #-}
rangeSetCheck :: RangeSet -> TMValueSimple -> Bool
rangeSetCheck RangeSet {..} val = V.foldl' f False _rsValues
    where f acc range = acc || checkRange val range
