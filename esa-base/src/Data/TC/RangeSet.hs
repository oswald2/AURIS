module Data.TC.RangeSet
    ( RangeSet(..)
    , RangeValue(..)
    , rangeSetCheck
    ) where

import           RIO
import qualified RIO.Vector                    as V
import           Data.Text.Short                ( ShortText )

import           Data.TM.Value
import           General.Types


data RangeValue =
  RangeDiscrete !TMValueSimple
  | RangeMinMax !TMValueSimple !TMValueSimple

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

{-# INLINABLE rangeSetCheck #-}
rangeSetCheck :: RangeSet -> TMValueSimple -> Bool
rangeSetCheck RangeSet {..} val = V.foldl' f False _rsValues
    where f acc range = acc || checkRange val range
