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
    ( Radix(..)
    , charToRadix
    , ValInter(..)
    , Correlate(..)
    , determineCorr
    , Endian(..)
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
    , hexBytesEmpty
    , parseHexLine
    , indentBuilder
    , newLineIndentBuilder
    , padRight
    , HasName(..)
    , HasNames(..)
    ) where


import           RIO
import qualified RIO.ByteString                as B
import qualified RIO.Text                      as T

import           Codec.Serialise               as S
import           Codec.Serialise.Decoding      as SE
import           Codec.Serialise.Encoding      as SE
import           Data.Aeson
import qualified Data.Aeson.Encoding           as E
import qualified Data.Aeson.Types              as E
import           Data.Attoparsec.Text           ( Parser )
import qualified Data.Attoparsec.Text          as A
import           Data.Bimap                    as BM
import           Data.Binary
import           Data.Bits                      ( Bits
                                                    ( (.&.)
                                                    , (.|.)
                                                    , shiftL
                                                    , shiftR
                                                    )
                                                )
import           Data.Char                      ( digitToInt
                                                , isHexDigit
                                                , isSpace
                                                )
import           Data.HashTable.ST.Basic        ( IHashTable )
import qualified Data.HashTable.ST.Basic       as HT
import           Data.Text.Short                ( ShortText )
import qualified Data.Text.Short               as ST
import qualified Text.Builder                  as TB
import           Text.Read                      ( Read(..) )

import           Control.Monad                  ( replicateM )

import           General.Hexdump



-- | The radix of a value
data Radix =
    Decimal
    | Octal
    | Hex
    deriving (Eq, Ord, Enum, Show, Read, Generic)

instance Serialise Radix
instance FromJSON Radix
instance ToJSON Radix where
    toEncoding = genericToEncoding defaultOptions
instance NFData Radix

instance Display Radix where
    textDisplay Decimal = "DEC"
    textDisplay Octal   = "OCT"
    textDisplay Hex     = "HEX"


-- | Converst from a Char to a 'Radix' according to the SCOS-2000 MIB ICD 6.9
{-# INLINABLE charToRadix #-}
charToRadix :: Char -> Radix
charToRadix 'D' = Decimal
charToRadix 'H' = Hex
charToRadix 'O' = Octal
charToRadix _   = Decimal


data ValInter = InterRaw | InterEng
  deriving (Eq, Ord, Enum, Read, Show, Generic)

instance Serialise ValInter
instance FromJSON ValInter
instance ToJSON ValInter where
    toEncoding = genericToEncoding defaultOptions
instance NFData ValInter

instance Display ValInter where
    textDisplay InterRaw = "RAW"
    textDisplay InterEng = "ENG"


-- | Specifies, if the parameter is a time value, if
-- this value should go through the time correlation
data Correlate = CorrelationYes | CorrelationNo
    deriving (Eq, Ord, Enum, Bounded, Show, Generic)

instance NFData Correlate
instance Serialise Correlate
instance FromJSON Correlate
instance ToJSON Correlate where
    toEncoding = genericToEncoding defaultOptions

instance Display Correlate where
    textDisplay CorrelationYes = "Correlation"
    textDisplay CorrelationNo  = "No Correlation"


{-# INLINABLE determineCorr #-}
determineCorr :: Maybe Bool -> Correlate
determineCorr = maybe CorrelationYes go
  where
    go True  = CorrelationYes
    go False = CorrelationNo

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
    display (BitSize x) = display x <> " Bit"

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
    encode x = S.encode . ST.toByteString $ x
    decode = do
        v <- S.decode
        case ST.fromByteString v of
            Just x -> return x
            Nothing ->
                fail
                    $  "Could not convert data from ByteString to ShortText: "
                    <> show v

-- instance FromJSON ShortText where
--     parseJSON = withText "ShortText" $ pure . ST.fromText

-- instance ToJSONKey ShortText where
--     toJSONKey = ToJSONKeyText ST.toText (E.text . ST.toText)

-- instance FromJSONKey ShortText where
--     fromJSONKey = FromJSONKeyText ST.fromText


-- instance ToJSON ShortText where
--     toJSON x = String . ST.toText $ x
--     {-# INLINE toJSON #-}

--     toEncoding x = E.text . ST.toText $ x
--     {-# INLINE toEncoding #-}

instance Display ShortText where
    display x = displayBytesUtf8 . ST.toByteString $ x


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
  deriving(Eq, Ord, Generic)

hexBytesEmpty :: HexBytes
hexBytesEmpty = HexBytes B.empty

hexLength :: HexBytes -> Int
hexLength (HexBytes x) = B.length x

parseHexLine :: Parser HexBytes
parseHexLine = do
    A.skip isSpace
    HexBytes . B.pack <$> many parseByte

parseByte :: Parser Word8
parseByte = do
    a <- A.satisfy isHexDigit
    b <- A.satisfy isHexDigit
    return $ fromIntegral (digitToInt a `shiftL` 4 .|. digitToInt b)

instance NFData HexBytes

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
        A.Fail{}       -> []
        A.Partial cont -> case cont T.empty of
            A.Fail{}      -> []
            A.Partial _   -> []
            A.Done rest x -> [(x, T.unpack rest)]
        A.Done rest x -> [(x, T.unpack rest)]




instance (ToJSON a, ToJSON b) => ToJSON (Bimap a b) where
    toJSON = toJSON . BM.toList


instance (FromJSON a, FromJSON b, Ord a, Ord b) => FromJSON (Bimap a b) where
    parseJSON x = BM.fromList <$> parseJSON x


instance (Serialise a, Serialise b, Ord a, Ord b) => Serialise (Bimap a b) where
    encode = S.encode . BM.toList
    decode = BM.fromList <$> S.decode



indentBuilder :: Word16 -> TB.Builder
indentBuilder n = TB.text (T.replicate (fromIntegral n) " ")

newLineIndentBuilder :: Word16 -> TB.Builder -> TB.Builder
newLineIndentBuilder n builder =
    TB.char '\n' <> indentBuilder n <> TB.padFromRight 23 ' ' builder


padRight :: Word16 -> TB.Builder -> TB.Builder
padRight n b = TB.padFromRight (fromIntegral n) ' ' b


class HasName a where
    getName :: a -> Text

class HasNames a where
    getNames :: a -> [Text]
