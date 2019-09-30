{-|
Module      : General.Types
Description : Provides general types and operations
Copyright   : (c) Michael Oswald, 2019
License     : BSD-3
Maintainer  : michael.oswald@onikudaki.net
Stability   : experimental
Portability : POSIX

This module provides some general data types and functions operating on them
-}
{-# LANGUAGE OverloadedStrings
    , BangPatterns
    , DeriveGeneric
    , GeneralizedNewtypeDeriving
    , RecordWildCards
    , NoImplicitPrelude
    , BinaryLiterals
    , NumericUnderscores
    , FlexibleInstances
    , GADTs
    , MultiParamTypeClasses
#-}
module General.Types
  ( Endian(..)
  , ByteOffset(..)
  , BitOffset(..)
  , Offset
  , BitOffsets(..)
  , mkByteOffset
  , unByteOffset
  , mkBitOffset
  , unBitOffset
  , mkOffset
  , ByteSize(..)
  , BitSize(..)
  , mkByteSize
  , mkBitSize
  , unBitSize
  , unByteSize
  , bytesToBitSize
  , bitSizeToBytes
  , nullOffset
  , OffsetCalculations(..)
  , addBitOffset
  , subBitOffset
  , nextByteAligned
  , isByteAligned
  , bitSizeToOffset
  , ToDouble(..)
  , splitBitOffset
  )
where


import           RIO
import           Data.Binary
import           Data.Aeson
import           Data.Bits

import           Codec.Serialise



-- | Specifies the endianess (BiE = Big Endian, LiE = Little Endian)
data Endian = BiE | LiE
    deriving (Eq, Ord, Enum, Show, Read, Generic)


instance Binary Endian
instance Serialise Endian
instance FromJSON Endian
instance ToJSON Endian where
    toEncoding = genericToEncoding defaultOptions
instance NFData Endian

-- | A byte offset
newtype ByteOffset = ByteOffset Int
    deriving (Eq, Ord, Num, Show, Read, Generic, NFData)

instance Binary ByteOffset
instance Serialise ByteOffset
instance FromJSON ByteOffset
instance ToJSON ByteOffset where
    toEncoding = genericToEncoding defaultOptions

-- | constructs a byte offset
mkByteOffset :: Int -> ByteOffset
mkByteOffset = ByteOffset

-- | extract the number out of a 'ByteOffset'
unByteOffset :: ByteOffset -> Int
unByteOffset (ByteOffset x) = x

-- | a bit offset
newtype BitOffset = BitOffset Int
    deriving (Eq, Ord, Num, Show, Read, Generic, NFData)

-- | constructs a bit offset
mkBitOffset :: Int -> BitOffset
mkBitOffset = BitOffset

-- | get the number out of the bit offset
unBitOffset :: BitOffset -> Int
unBitOffset (BitOffset x) = x

-- | splits the bit offset in the byte offset and the bits
splitBitOffset :: BitOffset -> (Int, Int)
splitBitOffset (BitOffset x) = (x `shiftR` 3, x .&. 0x07)

-- | a general offset, which contains a byte offset and a bit offset
data Offset = Offset ByteOffset BitOffset
    deriving (Eq, Show, Read, Generic)

instance NFData Offset

-- | constructs an 'Offset' from a 'ByteOffset' and a 'BitOffset'
mkOffset :: ByteOffset -> BitOffset -> Offset
mkOffset (ByteOffset x) (BitOffset y) =
  let bo = x + y `shiftR` 3
      bi = y .&. 0x07
  in  Offset (ByteOffset bo) (BitOffset bi)

-- | a null offset
nullOffset :: Offset
nullOffset = Offset (ByteOffset 0) (BitOffset 0)


instance Ord Offset where
  compare (Offset b1 bi1) (Offset b2 bi2) = case compare b1 b2 of
    LT -> LT
    GT -> GT
    EQ -> compare bi1 bi2


class ByteAligned a where
    -- | returns if the 'Offset' is byte aligned
    isByteAligned :: a -> Bool
    -- | Returns the next 'Offset' which is aligned to byte boundary
    nextByteAligned :: a -> a


instance ByteAligned Offset where
  nextByteAligned off@(Offset (ByteOffset a) (BitOffset _)) =
    if isByteAligned off then off else Offset (ByteOffset (a + 1)) (BitOffset 0)
  isByteAligned (Offset _ (BitOffset b)) = b == 0


instance ByteAligned BitOffset where
  nextByteAligned off@(BitOffset x) =
    if x .&. 0x7 /= 0 then BitOffset (((x `shiftR` 3) + 1) `shiftL` 3) else off
  isByteAligned (BitOffset x) = x .&. 0x7 == 0

instance ByteAligned BitSize where
  nextByteAligned off@(BitSize x) =
    if x .&. 0x7 /= 0 then BitSize (((x `shiftR` 3) + 1) `shiftL` 3) else off
  isByteAligned (BitSize x) = x .&. 0x7 == 0

-- | A size type in bytes
newtype ByteSize = ByteSize Int
    deriving (Eq, Ord, Num, Bits, Show, Read, Generic, NFData)

-- | constructs a byte size
mkByteSize :: Int -> ByteSize
mkByteSize = ByteSize

-- | extract the number out of a 'ByteSize'
unByteSize :: ByteSize -> Int
unByteSize (ByteSize x) = x



-- | A size type in bits
newtype BitSize = BitSize Int
    deriving (Eq, Ord, Num, Bits, Show, Read, Generic, NFData)

instance Binary BitSize
instance Serialise BitSize
instance FromJSON BitSize
instance ToJSON BitSize where
    toEncoding = genericToEncoding defaultOptions

-- | constructs a bit size
mkBitSize :: Int -> BitSize
mkBitSize = BitSize

-- | extract the number out of a 'BitSize'
unBitSize :: BitSize -> Int
unBitSize (BitSize x) = x

-- | converst a byte size into a bit size
bytesToBitSize :: ByteSize -> BitSize
bytesToBitSize (ByteSize x) = BitSize (x `shiftL` 3)

-- | converts a bit size into a byte size. Non 8-bit divisible values will
-- be truncated
bitSizeToBytes :: BitSize -> ByteSize
bitSizeToBytes (BitSize x) = ByteSize (x `shiftR` 3)

-- | Converst a 'BitSize' to a 'BitOffset', basically doing the subtraction of 1
bitSizeToOffset :: BitSize -> BitOffset
bitSizeToOffset (BitSize x) = BitOffset (x - 1)



-- | adds a 'BitOffset' and a 'BitSize' and returns a 'BitOffset'
addBitOffset :: BitOffset -> BitSize -> BitOffset
addBitOffset (BitOffset x) (BitSize s) = BitOffset (x + s)

-- | subtracts a 'BitSize' from a 'BitOffset' and returns a 'BitOffset'
subBitOffset :: BitOffset -> BitSize -> BitOffset
subBitOffset (BitOffset x) (BitSize s) = BitOffset (x - s)

-- | This class provides some conversion functions and operations on offsets
class BitOffsets a where
    toByteOffset :: a -> ByteOffset
    toBitOffset :: a -> BitOffset
    toOffset :: a -> Offset
    fromByteOffset :: ByteOffset -> a
    fromBitOffset :: BitOffset -> a

    addOff :: a -> a -> a
    subOff :: a -> a -> a


instance BitOffsets ByteOffset where
  toByteOffset x = x
  toBitOffset (ByteOffset x) = BitOffset (x `shiftL` 3)
  toOffset x = Offset x (BitOffset 0)
  fromByteOffset x = x
  fromBitOffset (BitOffset x) = ByteOffset (x `shiftR` 3)
  addOff (ByteOffset a) (ByteOffset b) = ByteOffset (a + b)
  subOff (ByteOffset a) (ByteOffset b) = ByteOffset (a - b)


instance BitOffsets BitOffset where
  toByteOffset (BitOffset x) = ByteOffset (x `shiftR` 3)
  toBitOffset x = x
  toOffset (BitOffset x) =
    let bo = x `shiftR` 3
        bi = x .&. 0x07
    in  Offset (ByteOffset bo) (BitOffset bi)
  fromByteOffset (ByteOffset x) = BitOffset (x `shiftL` 3)
  fromBitOffset x = x
  addOff (BitOffset a) (BitOffset b) = BitOffset (a + b)
  subOff (BitOffset a) (BitOffset b) = BitOffset (a - b)

instance BitOffsets Offset where
  toByteOffset (Offset bo _) = bo
  toBitOffset (Offset bo bi) = toBitOffset bo `addOff` bi
  toOffset x = x
  fromByteOffset x = Offset x (BitOffset 0)
  fromBitOffset (BitOffset x) =
    Offset (ByteOffset (x `shiftR` 3)) (BitOffset (x .&. 0x07))
  addOff (Offset (ByteOffset ba) (BitOffset bia)) (Offset (ByteOffset bb) (BitOffset bib))
    = let newBitOff' = bia + bib
          newByteOff = ba + bb + addBy
          addBy      = newBitOff' `shiftR` 3
          newBitOff  = newBitOff' .&. 0x07
      in  Offset (ByteOffset newByteOff) (BitOffset newBitOff)
  subOff off1 off2 = toOffset (toBitOffset off1 - toBitOffset off2)


class OffsetCalculations a b where
    (.+.) :: a -> b -> Offset
    (.-.) :: a -> b -> Offset

infixl 6  .+., .-.

instance OffsetCalculations BitOffset Offset where
  biOff .+. off = toOffset biOff `addOff` off
  biOff .-. off = toOffset biOff `subOff` off

instance OffsetCalculations Offset BitOffset where
  off .+. biOff = off `addOff` toOffset biOff
  off .-. biOff = off `subOff` toOffset biOff


instance OffsetCalculations Offset BitSize where
  off .+. (BitSize x) = off `addOff` toOffset (BitOffset x)
  off .-. (BitSize x) = off `subOff` toOffset (BitOffset x)



class ToDouble a where
    toDouble :: a -> Double

