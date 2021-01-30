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
{-# OPTIONS_GHC -fno-warn-orphans #-}
module General.Types
    ( Endian(..)
    , endianBuilder
    , ByteOffset(..)
    , BitOffset(..)
    , Offset
    , BitOffsets(..)
    , mkByteOffset
    , unByteOffset
    , mkBitOffset
    , unBitOffset
    , mkOffset
    , offsetParts
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
    , FromDouble(..)
    , splitBitOffset
    , encodeHashTable
    , decodeHashTable
    , HexBytes(..)
    , hexLength
    ) where


import           RIO
import           Data.Binary
import qualified RIO.ByteString                as B
import qualified RIO.Text                      as T
import           Data.Aeson                     ( withText
                                                , defaultOptions
                                                , genericToEncoding
                                                , FromJSON(..)
                                                , FromJSONKey(fromJSONKey)
                                                , FromJSONKeyFunction
                                                    ( FromJSONKeyText
                                                    )
                                                , Value(String)
                                                , ToJSON(toJSON, toEncoding)
                                                , ToJSONKey(toJSONKey)
                                                , ToJSONKeyFunction
                                                    ( ToJSONKeyText
                                                    )
                                                )
import qualified Data.Aeson.Encoding           as E
import qualified Data.Aeson.Types              as E
import           Data.Bits                      ( Bits
                                                    ( (.|.)
                                                    , (.&.)
                                                    , shiftR
                                                    , shiftL
                                                    )
                                                )
import           Data.Text.Short                ( ShortText )
import qualified Data.Text.Short               as ST
import           Data.HashTable.ST.Basic        ( IHashTable )
import qualified Data.HashTable.ST.Basic       as HT
import           Data.Attoparsec.Text           ( Parser )
import qualified Data.Attoparsec.Text          as A
import           Data.Char                      ( ord
                                                , isHexDigit
                                                )
import           Text.Read                      ( Read(..) )
import qualified Text.Builder                  as TB
import           Codec.Serialise               as S
import           Codec.Serialise.Encoding      as SE
import           Codec.Serialise.Decoding      as SE

import           Control.Monad                  ( replicateM )

import           General.Hexdump




-- | Specifies the endianess (BiE = Big Endian, LiE = Little Endian)
data Endian = BiE | LiE
    deriving (Eq, Ord, Enum, Show, Read, Generic)


instance Binary Endian
instance Serialise Endian
instance FromJSON Endian
instance ToJSON Endian where
    toEncoding = genericToEncoding defaultOptions
instance NFData Endian

instance Display Endian where
    display BiE = "BE"
    display LiE = "LE"

endianBuilder :: Endian -> TB.Builder
endianBuilder BiE = TB.text "BE"
endianBuilder LiE = TB.text "LE"


-- | A byte offset
newtype ByteOffset = ByteOffset Int
    deriving (Eq, Ord, Num, Show, Read, Generic, NFData)

instance Binary ByteOffset
instance Serialise ByteOffset
instance FromJSON ByteOffset
instance ToJSON ByteOffset where
    toEncoding = genericToEncoding defaultOptions


instance Display ByteOffset where
    display (ByteOffset x) = display x

-- | constructs a byte offset
mkByteOffset :: Int -> ByteOffset
mkByteOffset = ByteOffset

-- | extract the number out of a 'ByteOffset'
unByteOffset :: ByteOffset -> Int
unByteOffset (ByteOffset x) = x

-- | a bit offset
newtype BitOffset = BitOffset Int
    deriving (Eq, Ord, Num, Show, Read, Generic, NFData)

instance Serialise BitOffset
instance FromJSON BitOffset
instance ToJSON BitOffset where
    toEncoding = genericToEncoding defaultOptions

instance Display BitOffset where
    display (BitOffset x) = display x

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
instance Serialise Offset
instance FromJSON Offset
instance ToJSON Offset where
    toEncoding = genericToEncoding defaultOptions


instance Display Offset where
    display (Offset b bi) =
        display ("Offset " :: Text) <> display b <> " " <> display bi

-- | constructs an 'Offset' from a 'ByteOffset' and a 'BitOffset'
mkOffset :: ByteOffset -> BitOffset -> Offset
mkOffset (ByteOffset x) (BitOffset y) =
    let bo = x + y `shiftR` 3
        bi = y .&. 0x07
    in  Offset (ByteOffset bo) (BitOffset bi)

-- | a null offset
nullOffset :: Offset
nullOffset = Offset (ByteOffset 0) (BitOffset 0)

-- | Return the parts constituting the 'Offset'
offsetParts :: Offset -> (ByteOffset, BitOffset)
offsetParts (Offset bo bi) = (bo, bi)


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
        if isByteAligned off
            then off
            else Offset (ByteOffset (a + 1)) (BitOffset 0)
    isByteAligned (Offset _ (BitOffset b)) = b == 0


instance ByteAligned BitOffset where
    nextByteAligned off@(BitOffset x) = if x .&. 0x7 /= 0
        then BitOffset (((x `shiftR` 3) + 1) `shiftL` 3)
        else off
    isByteAligned (BitOffset x) = x .&. 0x7 == 0

instance ByteAligned BitSize where
    nextByteAligned off@(BitSize x) = if x .&. 0x7 /= 0
        then BitSize (((x `shiftR` 3) + 1) `shiftL` 3)
        else off
    isByteAligned (BitSize x) = x .&. 0x7 == 0

-- | A size type in bytes
newtype ByteSize = ByteSize Int
    deriving (Eq, Ord, Num, Bits, Show, Read, Generic, NFData)

instance Display ByteSize where
    display (ByteSize x) = display ("ByteSize " :: Text) <> display x

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

instance Display BitSize where
    display (BitSize x) = display ("BitSize " :: Text) <> display x

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

-- | Class for offset calculations
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


-- | Convert a value to a 'Double'
class ToDouble a where
    toDouble :: a -> Double

-- | Convert a value from a 'Double'
class FromDouble a where
    fromDouble :: Double -> a



instance Serialise ShortText where
    encode = S.encode . ST.toText
    decode = ST.fromText <$> S.decode

instance FromJSON ShortText where
    parseJSON = withText "ShortText" $ pure . ST.fromText

instance ToJSONKey ShortText where
    toJSONKey = ToJSONKeyText ST.toText (E.text . ST.toText)

instance FromJSONKey ShortText where
    fromJSONKey = FromJSONKeyText ST.fromText


instance ToJSON ShortText where
    toJSON = String . ST.toText
    {-# INLINE toJSON #-}

    toEncoding = E.text . ST.toText
    {-# INLINE toEncoding #-}

instance Display ShortText where
    display = display . ST.toText


-- | Serialise a 'IHashTable' 
encodeHashTable :: (Serialise k, Serialise v) => IHashTable k v -> SE.Encoding
encodeHashTable ht =
    let lst = HT.toList ht
    in  foldl' (\enc (k, v) -> enc <> S.encode k <> S.encode v)
               (SE.encodeListLen (fromIntegral (length lst)))
               (HT.toList ht)

-- | Deserialise a 'IHashTable'
decodeHashTable
    :: (Serialise k, Serialise v, Eq k, Hashable k)
    => Decoder s (IHashTable k v)
decodeHashTable = do
    len <- SE.decodeListLen
    lst <- replicateM len ((,) <$> S.decode <*> S.decode)
    return (HT.fromList lst)


instance (ToJSON k, ToJSON v) => ToJSON (IHashTable k v) where
    toJSON ht = let lst = HT.toList ht in toJSON lst
    toEncoding ht = let lst = HT.toList ht in E.list toEncoding lst

instance (Eq k, Hashable k, FromJSON k, FromJSON v) => FromJSON (IHashTable k v) where
    parseJSON x = HT.fromList <$> parseJSONList x


-- | A newtype wrapper around 'ByteString' for text-serialising a 'ByteString'
-- into a hex-coded string value
newtype HexBytes = HexBytes { toBS :: ByteString }
  deriving(Eq)


hexLength :: HexBytes -> Int
hexLength (HexBytes x) = B.length x

parseHexLine :: Parser HexBytes
parseHexLine = do
    HexBytes . B.pack <$> many parseByte

parseByte :: Parser Word8
parseByte = do
    a <- A.satisfy isHexDigit
    b <- A.satisfy isHexDigit
    return $ fromIntegral (ord a `shiftL` 4 .|. ord b)



instance Serialise HexBytes where
    encode (HexBytes b) = S.encode b
    decode = HexBytes <$> S.decode

instance ToJSON HexBytes where
    toJSON (HexBytes b) = String $ hexdumpLineNoSpace b
    {-# INLINE toJSON #-}

    toEncoding (HexBytes b) = E.text $ hexdumpLineNoSpace b
    {-# INLINE toEncoding #-}

instance FromJSON HexBytes where
    parseJSON (String t) = case A.parseOnly parseHexLine t of
        Left  err -> fail err
        Right x   -> return x
    parseJSON invalid = E.prependFailure "parsing HexBytes failed, "
                                         (E.typeMismatch "String" invalid)

instance Display HexBytes where
    display (HexBytes str) = display $ hexdumpLineNoSpace str

instance Show HexBytes where
    show (HexBytes b) = T.unpack $ hexdumpLineNoSpace b

instance Read HexBytes where
    readsPrec _ str = case A.parse parseHexLine (T.pack str) of
        A.Fail{}      -> []
        A.Partial _   -> []
        A.Done rest x -> [(x, T.unpack rest)]
