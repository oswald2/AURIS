{-# LANGUAGE CPP #-}

module Data.HashTable.Internal.Array
  ( MutableArray
  , Array
  , newArray
  , readArray
  , writeArray
  , unsafeFreezeArray
  , ireadArray
  ) where

import Prelude

import           Control.Monad.ST
#ifdef BOUNDS_CHECKING
import qualified Data.Vector.Mutable as M
import           Data.Vector.Mutable (MVector)
import qualified Data.Vector as IV
import           Data.Vector (Vector)
#else
import qualified Data.Primitive.Array as M
import           Data.Primitive.Array (MutableArray, Array)
#endif


#ifdef BOUNDS_CHECKING

type MutableArray s a = MVector s a

type Array a = Vector a

newArray :: Int -> a -> ST s (MutableArray s a)
newArray = M.replicate

readArray :: MutableArray s a -> Int -> ST s a
readArray = M.read

writeArray :: MutableArray s a -> Int -> a -> ST s ()
writeArray = M.write

unsafeFreezeArray :: MutableArray s a -> ST s (Array a)
unsafeFreezeArray = M.unsafeFreeze

ireadArray :: Array a -> Int -> a
ireadArray = IV.read

#else

newArray :: Int -> a -> ST s (MutableArray s a)
newArray = M.newArray

readArray :: MutableArray s a -> Int -> ST s a
readArray = M.readArray

writeArray :: MutableArray s a -> Int -> a -> ST s ()
writeArray = M.writeArray

unsafeFreezeArray :: MutableArray s a -> ST s (Array a)
unsafeFreezeArray = M.unsafeFreezeArray

ireadArray :: Array a -> Int -> a
ireadArray = M.indexArray

#endif
