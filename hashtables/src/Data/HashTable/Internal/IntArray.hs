{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP          #-}
{-# LANGUAGE MagicHash    #-}

module Data.HashTable.Internal.IntArray
  ( IntArray
  , IIntArray
  , Elem
  , elemMask
  , primWordToElem
  , elemToInt
  , elemToInt#
  , newArray
  , readArray
  , writeArray
  , length
  , toPtr
  , itoPtr
  , unsafeFreezeIntArray
  , ireadArray
  ) where

------------------------------------------------------------------------------
import           Control.Monad.ST
import           Data.Bits
import qualified Data.Primitive.ByteArray as A
#if !MIN_VERSION_primitive(0,7,0)
import           Data.Primitive.Types     (Addr (..))
#endif
import           GHC.Exts
import           GHC.Word
import           Prelude                  hiding (length)
------------------------------------------------------------------------------


#ifdef BOUNDS_CHECKING
#define BOUNDS_MSG(sz,i) concat [ "[", __FILE__, ":",                         \
                                  show (__LINE__ :: Int),                     \
                                  "] bounds check exceeded: ",                \
                                  "size was ", show (sz), " i was ", show (i) ]

#define BOUNDS_CHECK(arr,i) let sz = (A.sizeofMutableByteArray (arr)          \
                                      `div` wordSizeInBytes) in               \
                            if (i) < 0 || (i) >= sz                           \
                              then error (BOUNDS_MSG(sz,(i)))                 \
                              else return ()
#else
#define BOUNDS_CHECK(arr,i)
#endif


------------------------------------------------------------------------------
newtype IntArray s = IA (A.MutableByteArray s)
type Elem = Word16

-- | Immutable version of IntArray
newtype IIntArray = IIA A.ByteArray

-- | unsafely freezes the 'IntArray' into a 'IIntArray'
unsafeFreezeIntArray :: IntArray s -> ST s IIntArray
unsafeFreezeIntArray (IA arr) = IIA <$> A.unsafeFreezeByteArray arr



------------------------------------------------------------------------------
primWordToElem :: Word# -> Elem
primWordToElem = W16#


------------------------------------------------------------------------------
elemToInt :: Elem -> Int
elemToInt e = let !i# = elemToInt# e
              in (I# i#)


------------------------------------------------------------------------------
elemToInt# :: Elem -> Int#
elemToInt# (W16# w#) = word2Int# w#


------------------------------------------------------------------------------
elemMask :: Int
elemMask = 0xffff


------------------------------------------------------------------------------
wordSizeInBytes :: Int
wordSizeInBytes = finiteBitSize (0::Elem) `div` 8


------------------------------------------------------------------------------
-- | Cache line size, in bytes
cacheLineSize :: Int
cacheLineSize = 64


------------------------------------------------------------------------------
newArray :: Int -> ST s (IntArray s)
newArray n = do
    let !sz = n * wordSizeInBytes
    !arr <- A.newAlignedPinnedByteArray sz cacheLineSize
    A.fillByteArray arr 0 sz 0
    return $! IA arr


------------------------------------------------------------------------------
readArray :: IntArray s -> Int -> ST s Elem
readArray (IA a) idx = do
    BOUNDS_CHECK(a,idx)
    A.readByteArray a idx

ireadArray :: IIntArray -> Int -> Elem
ireadArray (IIA a) idx = A.indexByteArray a idx


------------------------------------------------------------------------------
writeArray :: IntArray s -> Int -> Elem -> ST s ()
writeArray (IA a) idx val = do
    BOUNDS_CHECK(a,idx)
    A.writeByteArray a idx val


------------------------------------------------------------------------------
length :: IntArray s -> Int
length (IA a) = A.sizeofMutableByteArray a `div` wordSizeInBytes


------------------------------------------------------------------------------
toPtr :: IntArray s -> Ptr a
toPtr (IA a) = Ptr a#
  where
#if MIN_VERSION_primitive(0,7,0)
    !(Ptr !a#) = A.mutableByteArrayContents a
#else
    !(Addr !a#) = A.mutableByteArrayContents a
#endif

itoPtr :: IIntArray -> Ptr a
itoPtr (IIA a) = Ptr a#
  where
#if MIN_VERSION_primitive(0,7,0)
    !(Ptr !a#) = A.byteArrayContents a
#else
    !(Addr !a#) = A.byteArrayContents a
#endif
