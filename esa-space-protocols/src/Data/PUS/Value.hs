{-# LANGUAGE 
    TemplateHaskell
#-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.PUS.Value
    ( Value(..)
    , initialValue
    , getByteOrder
    , getInt
    , setInt
    , isSetableAligned
    , setAlignedValue
    , getAlignedValue
    , getUnalignedValue
    , isStorableWord64
    , isGettableUnaligned
    , valueBuilder
    , valueHeaderLine
    , B8(..)
    ) where

import           RIO
import qualified RIO.ByteString                as B
import qualified RIO.Text                      as T
import qualified RIO.HashMap                   as HM
import qualified RIO.Vector                    as V
import qualified RIO.Vector.Partial            as V
                                                ( (!) )
import qualified RIO.Vector.Storable           as VS

import qualified Data.ByteString.Char8         as BC
import           Data.Int.Int24
import           Data.Word.Word24
import           Data.Bits
import           Data.Attoparsec.ByteString    as A
                                                ( parseOnly )
import           Data.Attoparsec.Binary        as A
                                                ( anyWord64be )
import           Data.Aeson                    as AE
                                                ( FromJSON(parseJSON)
                                                , ToJSON(toJSON, toEncoding)
                                                , pairs
                                                , (.:)
                                                , object
                                                , KeyValue((.=))
                                                )
import qualified Data.Aeson                    as AE
                                                ( Value(String, Object) )
import           Data.ByteString.Base64.Type    ( getByteString64
                                                , makeByteString64
                                                )
import           Data.Attoparsec.ByteString.Char8
                                               as A
                                                ( decimal )
import           ByteString.StrictBuilder       ( asciiIntegral
                                                , builderBytes
                                                , word64BE
                                                )

import           Codec.Serialise                ( Serialise(..) )
import           Codec.Serialise.Encoding       ( encodeWord8
                                                , encodeWord16
                                                , encodeWord32
                                                , encodeInt32
                                                )
import           Codec.Serialise.Decoding       ( decodeWord8
                                                , decodeWord16
                                                , decodeWord32
                                                , decodeInt32
                                                )

import           Data.PUS.EncTime               ( cucTimeLen
                                                , cucTimeSetDelta
                                                , nullCUCTime
                                                , nullCUCTimeRel
                                                , CUCTime(..)
                                                )

import           General.SizeOf                 ( BitSizes(..) )

import           General.Hexdump                ( hexdumpLineBS )
import           General.Types                  
import           General.PUSTypes               ( PFC(..)
                                                , PTC(..)
                                                )
import           General.Time                   ( TimeRepConversion
                                                    ( timeToMicro
                                                    , microToTime
                                                    )
                                                )
import           General.SetBitField            ( SetValue(setValue)
                                                , copyBS
                                                )
import           General.GetBitField            ( GetValue(getValue)
                                                , getBitFieldDouble
                                                , getBitFieldInt64
                                                , getBitFieldWord64
                                                , getValueOctet
                                                , getValueOctetLen
                                                )
import           General.Padding                ( leftPaddedC
                                                , rightPadded
                                                , rightPaddedC
                                                , padFromRight
                                                )
import           General.Chunks
import qualified Text.Builder                  as TB
import           Refined

-- | Refinement type for the bit width of the values. Allowed are numbers 1 to 7
--   (for the bit widths of 1 to 7 bit)
newtype B8 = B8 (Refined (And (Not (LessThan 1)) (Not (GreaterThan 7))) Word8)
    deriving(Eq, Show, Read, ToJSON, FromJSON, NFData, Generic)


-- | Return the number of bits in the width
unB8 :: B8 -> Word8
unB8 (B8 x) = unrefine x

instance Serialise B8 where
    encode (B8 x) = encodeWord8 (unrefine x)
    decode = do
        x <- decodeWord8
        B8 <$> refineFail x


-- | Refinement type for the bit width of the values. Allowed are numbers 1 to 15
--   (for the bit widths of 1 to 15 bit)
newtype B16 = B16 (Refined (And (Not (LessThan 1)) (Not (GreaterThan 15))) Word8)
    deriving(Eq, Show, Read, ToJSON, FromJSON, NFData, Generic)

unB16 :: B16 -> Word8
unB16 (B16 x) = unrefine x

instance Serialise B16 where
    encode (B16 x) = encodeWord8 (unrefine x)
    decode = do
        x <- decodeWord8
        B16 <$> refineFail x


-- | Refinement type for the bit width of the values. Allowed are numbers 1 to 31
--   (for the bit widths of 1 to 31 bit)
newtype B32 = B32 (Refined (And (Not (LessThan 1)) (Not (GreaterThan 31))) Word8)
    deriving(Eq, Show, Read, ToJSON, FromJSON, NFData, Generic)

unB32 :: B32 -> Word8
unB32 (B32 x) = unrefine x

instance Serialise B32 where
    encode (B32 x) = encodeWord8 (unrefine x)
    decode = do
        x <- decodeWord8
        B32 <$> refineFail x



instance Serialise Word24 where
    encode x = encodeWord32 (fromIntegral x)
    decode = fromIntegral <$> decodeWord32

instance Serialise Int24 where
    encode x = encodeInt32 (fromIntegral x)
    decode = fromIntegral <$> decodeInt32

instance ToJSON Word24 where
    toJSON x =
        let y :: Word32
            y = fromIntegral x
        in  object ["value" .= y]
    toEncoding x =
        let y :: Word32
            y = fromIntegral x
        in  pairs ("value" .= y)

instance ToJSON Int24 where
    toJSON x =
        let y :: Int32
            y = fromIntegral x
        in  object ["value" .= y]
    toEncoding x =
        let y :: Int32
            y = fromIntegral x
        in  pairs ("value" .= y)



instance FromJSON Word24 where
    parseJSON (AE.Object o) = do
        x <- o .: "value"
        return (fromIntegral (x :: Word32))
    parseJSON _ = fail "FromJSON Word24: expected JSON object"

instance FromJSON Int24 where
    parseJSON (AE.Object o) = do
        x <- o .: "value"
        return (fromIntegral (x :: Int32))
    parseJSON _ = fail "FromJSON Int24: expected JSON object"


instance Display Word24 where
    display x = display (fromIntegral x :: Word32)

instance Display Int24 where
    display x = display (fromIntegral x :: Int32)



data Value =
    ValInt8 !Int8
    | ValInt16 Endian !Int16
    | ValInt24 Endian !Int24
    | ValInt32 Endian !Int32
    | ValInt64 Endian !Int64
    | ValUInt8X !B8 !Word8
    | ValUInt8 !Word8
    | ValUInt16X !B16 !Word16
    | ValUInt16 Endian !Word16
    | ValUInt24 Endian !Word24
    | ValUInt32X !B32 !Word32
    | ValUInt32 Endian !Word32
    | ValUInt64 Endian !Word64
    | ValDouble Endian !Double
    | ValString !ByteString
    | ValFixedString !Word16 !ByteString
    | ValOctet !ByteString
    | ValFixedOctet !Word16 !ByteString
    | ValCUCTime CUCTime
    | ValUndefined
    deriving (Eq, Read, Show, Generic, NFData)


instance Serialise Value


instance AE.ToJSON Value where
    toJSON (ValInt8 x) =
        object ["valType" .= ("ValInt8" :: Text), "value" .= x]
    toJSON (ValInt16 b x) =
        object
            [ "valType" .= ("ValInt16" :: Text)
            , "value" .= x
            , "endian" .= toJSON b
            ]
    toJSON (ValInt24 b x) =
        object
            [ "valType" .= ("ValInt24" :: Text)
            , "value" .= x
            , "endian" .= toJSON b
            ]
    toJSON (ValInt32 b x) =
        object
            [ "valType" .= ("ValInt32" :: Text)
            , "value" .= x
            , "endian" .= toJSON b
            ]
    toJSON (ValInt64 b x) =
        object
            [ "valType" .= ("ValInt64" :: Text)
            , "value" .= x
            , "endian" .= toJSON b
            ]
    toJSON (ValUInt8X w x) =
        object ["valType" .= ("ValUIntX" :: Text), "value" .= x, "width" .= w]
    toJSON (ValUInt8 x) =
        object ["valType" .= ("ValUInt8" :: Text), "value" .= x]
    toJSON (ValUInt16X w x) =
        object ["valType" .= ("ValUInt16" :: Text), "value" .= x, "width" .= w]
    toJSON (ValUInt16 e x) =
        object ["valType" .= ("ValUInt16" :: Text), "value" .= x, "endian" .= e]
    toJSON (ValUInt24 e x) =
        object ["valType" .= ("ValUInt24" :: Text), "value" .= x, "endian" .= e]
    toJSON (ValUInt32X w x) =
        object ["valType" .= ("ValUInt32X" :: Text), "value" .= x, "width" .= w]
    toJSON (ValUInt32 e x) =
        object ["valType" .= ("ValUInt32" :: Text), "value" .= x, "endian" .= e]
    toJSON (ValUInt64 e x) =
        object ["valType" .= ("ValUInt64" :: Text), "value" .= x, "endian" .= e]
    toJSON (ValDouble b x) =
        object
            [ "valType" .= ("ValDouble" :: Text)
            , "value" .= x
            , "endian" .= toJSON b
            ]
    toJSON (ValString x) = object
        [ "valType" .= ("ValString" :: Text)
        , "value" .= case T.decodeUtf8' x of
            Left  _   -> "ERROR: invalide UTF-8 code"
            Right val -> val
        ]
    toJSON (ValFixedString width x) = object
        [ "valType" .= ("ValFixedString" :: Text)
        , "value" .= case T.decodeUtf8' x of
            Left  _   -> "ERROR: invalide UTF-8 code"
            Right val -> val
        , "width" .= width
        ]
    toJSON (ValOctet x) = object
        ["valType" .= ("ValOctet" :: Text), "value" .= makeByteString64 x]
    toJSON (ValFixedOctet width x) = object
        [ "valType" .= ("ValFixedOctet" :: Text)
        , "value" .= makeByteString64 x
        , "width" .= width
        ]
    toJSON (ValCUCTime x) =
        object ["valType" .= ("ValCUCTime" :: Text), "value" .= toJSON x]
    toJSON ValUndefined = object ["valType" .= ("ValUndefined" :: Text)]

    toEncoding (ValInt8 x) =
        pairs ("valType" .= ("ValInt8" :: Text) <> "value" .= x)
    toEncoding (ValInt16 b x) = pairs
        (  "valType"
        .= ("ValInt16" :: Text)
        <> "value"
        .= x
        <> "endian"
        .= toJSON b
        )
    toEncoding (ValInt24 b x) = pairs
        (  "valType"
        .= ("ValInt24" :: Text)
        <> "value"
        .= x
        <> "endian"
        .= toJSON b
        )
    toEncoding (ValInt32 b x) = pairs
        (  "valType"
        .= ("ValInt32" :: Text)
        <> "value"
        .= x
        <> "endian"
        .= toJSON b
        )
    toEncoding (ValInt64 b x) = pairs
        (  "valType"
        .= ("ValInt64" :: Text)
        <> "value"
        .= x
        <> "endian"
        .= toJSON b
        )
    toEncoding (ValUInt8X w x) = pairs
        ("valType" .= ("ValUInt3" :: Text) <> "value" .= x <> "width" .= w)
    toEncoding (ValUInt8 x) =
        pairs ("valType" .= ("ValUInt8" :: Text) <> "value" .= x)
    toEncoding (ValUInt16X w x) = pairs
        ("valType" .= ("ValUInt16" :: Text) <> "value" .= x <> "width" .= w)
    toEncoding (ValUInt16 e x) = pairs
        (  "valType"
        .= ("ValUInt16" :: Text)
        <> "value"
        .= x
        <> "endian"
        .= toJSON e
        )
    toEncoding (ValUInt32X w x) = pairs
        (  "valType"
        .= ("ValUInt32" :: Text)
        <> "value"
        .= x
        <> "width"
        .= toJSON w
        )
    toEncoding (ValUInt24 e x) = pairs
        (  "valType"
        .= ("ValUInt24" :: Text)
        <> "value"
        .= x
        <> "endian"
        .= toJSON e
        )
    toEncoding (ValUInt32 e x) = pairs
        (  "valType"
        .= ("ValUInt32" :: Text)
        <> "value"
        .= x
        <> "endian"
        .= toJSON e
        )
    toEncoding (ValUInt64 e x) = pairs
        (  "valType"
        .= ("ValUInt64" :: Text)
        <> "value"
        .= x
        <> "endian"
        .= toJSON e
        )
    toEncoding (ValDouble b x) = pairs
        (  "valType"
        .= ("ValDouble" :: Text)
        <> "value"
        .= x
        <> "endian"
        .= toJSON b
        )
    toEncoding (ValString x) = pairs
        (  "valType"
        .= ("ValString" :: Text)
        <> "value"
        .= case T.decodeUtf8' x of
               Left  _   -> "ERROR: invalide UTF-8 code"
               Right val -> val
        )
    toEncoding (ValFixedString width x) = pairs
        (  "valType"
        .= ("ValFixedString" :: Text)
        <> "value"
        .= case T.decodeUtf8' x of
               Left  _   -> "ERROR: invalide UTF-8 code"
               Right val -> val
        <> "width"
        .= width
        )
    toEncoding (ValOctet x) = pairs
        ("valType" .= ("ValOctet" :: Text) <> "value" .= makeByteString64 x)
    toEncoding (ValFixedOctet width x) = pairs
        (  "valType"
        .= ("ValFixedOctet" :: Text)
        <> "value"
        .= makeByteString64 x
        <> "width"
        .= width
        )
    toEncoding (ValCUCTime x) =
        pairs ("valType" .= ("ValCUCTime" :: Text) <> "value" .= toJSON x)
    toEncoding ValUndefined = pairs ("valType" .= ("ValUndefined" :: Text))

instance FromJSON Value where
    parseJSON (AE.Object o) = case HM.lookup "valType" o of
        Just (AE.String "ValInt8") -> ValInt8 <$> o .: "value"
        Just (AE.String "ValInt16") ->
            ValInt16 <$> o .: "endian" <*> o .: "value"
        Just (AE.String "ValInt24") ->
            ValInt24 <$> o .: "endian" <*> o .: "value"
        Just (AE.String "ValInt32") ->
            ValInt32 <$> o .: "endian" <*> o .: "value"
        Just (AE.String "ValInt64") ->
            ValInt64 <$> o .: "endian" <*> o .: "value"
        Just (AE.String "ValUInt8X") -> do
            ValUInt8X <$> o .: "width" <*> o .: "value"
        Just (AE.String "ValUInt8") -> ValUInt8 <$> o .: "value"
        Just (AE.String "ValUInt16X") ->
            ValUInt16X <$> o .: "width" <*> o .: "value"
        Just (AE.String "ValUInt16") ->
            ValUInt16 <$> o .: "endian" <*> o .: "value"
        Just (AE.String "ValUInt32") ->
            ValUInt32 <$> o .: "endian" <*> o .: "value"
        Just (AE.String "ValUInt64") ->
            ValUInt64 <$> o .: "endian" <*> o .: "value"
        Just (AE.String "ValDouble") ->
            ValDouble <$> o .: "endian" <*> o .: "value"
        Just (AE.String "ValString") -> ValString . encodeUtf8 <$> o .: "value"
        Just (AE.String "ValFixedString") ->
            ValFixedString <$> o .: "width" <*> (encodeUtf8 <$> o .: "value")
        Just (AE.String "ValOctet") ->
            ValOctet . getByteString64 <$> o .: "value"
        Just (AE.String "ValFixedOctet") ->
            ValFixedOctet
                <$> o
                .:  "width"
                <*> (getByteString64 <$> o .: "value")
        Just (AE.String "ValCUCTime"  ) -> ValCUCTime <$> o .: "value"
        Just (AE.String "ValUndefined") -> pure ValUndefined
        _                               -> pure ValUndefined
    parseJSON _ = pure ValUndefined



{-# INLINABLE initialValue #-}
initialValue :: Endian -> PTC -> PFC -> Value
initialValue _ (PTC 1) (PFC 0) = ValUInt8X (B8 $$(refineTH 1)) 0
initialValue _ (PTC 2) (PFC x) = case refine (fromIntegral x) of
    Left  _ -> ValUndefined
    Right w -> ValUInt32X (B32 w) 0
initialValue _ (PTC 3) (PFC 0 ) = ValUInt8X (B8 $$(refineTH 4)) 0
initialValue _ (PTC 3) (PFC 1 ) = ValUInt8X (B8 $$(refineTH 5)) 0
initialValue _ (PTC 3) (PFC 2 ) = ValUInt8X (B8 $$(refineTH 6)) 0
initialValue _ (PTC 3) (PFC 3 ) = ValUInt8X (B8 $$(refineTH 7)) 0
initialValue _ (PTC 3) (PFC 4 ) = ValUInt8 0
initialValue _ (PTC 3) (PFC 5 ) = ValUInt16X (B16 $$(refineTH 9)) 0
initialValue _ (PTC 3) (PFC 6 ) = ValUInt16X (B16 $$(refineTH 10)) 0
initialValue _ (PTC 3) (PFC 7 ) = ValUInt16X (B16 $$(refineTH 11)) 0
initialValue _ (PTC 3) (PFC 8 ) = ValUInt16X (B16 $$(refineTH 12)) 0
initialValue _ (PTC 3) (PFC 9 ) = ValUInt16X (B16 $$(refineTH 13)) 0
initialValue _ (PTC 3) (PFC 10) = ValUInt16X (B16 $$(refineTH 14)) 0
initialValue _ (PTC 3) (PFC 11) = ValUInt16X (B16 $$(refineTH 15)) 0
initialValue b (PTC 3) (PFC 12) = ValUInt16 b 0
initialValue b (PTC 3) (PFC 13) = ValUInt24 b 0
initialValue b (PTC 3) (PFC 14) = ValUInt32 b 0
initialValue b (PTC 3) (PFC 16) = ValUInt64 b 0
initialValue _ (PTC 4) (PFC 4 ) = ValInt8 0
initialValue b (PTC 4) (PFC 12) = ValInt16 b 0
initialValue b (PTC 4) (PFC 13) = ValInt24 b 0
initialValue b (PTC 4) (PFC 14) = ValInt32 b 0
initialValue b (PTC 4) (PFC 16) = ValInt64 b 0
initialValue b (PTC 5) (PFC 2 ) = ValDouble b 0.0
initialValue _ (PTC 8) (PFC 0 ) = ValString B.empty
initialValue _ (PTC 8) (PFC x) =
    ValFixedString (fromIntegral x) (BC.replicate x ' ')
initialValue _ (PTC 7) (PFC 0) = ValOctet B.empty
initialValue _ (PTC 7) (PFC x) =
    ValFixedOctet (fromIntegral x) (B.replicate x 0)
initialValue _ (PTC 9 ) (PFC _) = ValCUCTime nullCUCTime
initialValue _ (PTC 10) (PFC _) = ValCUCTime nullCUCTimeRel
initialValue _ _        _       = ValUndefined


instance BitSizes Value where
    {-# INLINABLE bitSize #-}
    bitSize ValInt8{}        = mkBitSize 8
    bitSize ValInt16{}       = mkBitSize 16
    bitSize ValInt24{}       = mkBitSize 24
    bitSize ValInt32{}       = mkBitSize 32
    bitSize ValInt64{}       = mkBitSize 64
    bitSize (ValUInt8X w _)  = mkBitSize (fromIntegral (unB8 w))
    bitSize ValUInt8{}       = mkBitSize 8
    bitSize (ValUInt16X w _) = mkBitSize (fromIntegral (unB16 w))
    bitSize ValUInt16{}      = mkBitSize 16
    bitSize (ValUInt32X w _) = mkBitSize (fromIntegral (unB32 w))
    bitSize ValUInt24{}      = mkBitSize 24
    bitSize ValUInt32{}      = mkBitSize 32
    bitSize ValUInt64{}      = mkBitSize 64
    bitSize ValDouble{}      = mkBitSize 64
    bitSize (ValString x)    = bytesToBitSize . mkByteSize $ B.length x
    bitSize (ValFixedString width _) =
        bytesToBitSize . mkByteSize $ fromIntegral width
    bitSize (ValOctet x) = bytesToBitSize . mkByteSize $ B.length x
    bitSize (ValFixedOctet width _) =
        bytesToBitSize . mkByteSize $ fromIntegral width
    bitSize (ValCUCTime _) = bytesToBitSize . mkByteSize $ cucTimeLen
    bitSize ValUndefined   = mkBitSize 0


{-# INLINABLE getByteOrder #-}
getByteOrder :: Value -> Maybe Endian
getByteOrder ValInt8{}        = Nothing
getByteOrder (ValInt16 b _)   = Just b
getByteOrder (ValInt24 b _)   = Just b
getByteOrder (ValInt32 b _)   = Just b
getByteOrder (ValInt64 b _)   = Just b
getByteOrder ValUInt8X{}      = Nothing
getByteOrder (ValUInt8 _)     = Nothing
getByteOrder ValUInt16X{}     = Nothing
getByteOrder (ValUInt16 b _)  = Just b
getByteOrder ValUInt32X{}     = Nothing
getByteOrder (ValUInt24 b _)  = Just b
getByteOrder (ValUInt32 b _)  = Just b
getByteOrder (ValUInt64 b _)  = Just b
getByteOrder (ValDouble b _)  = Just b
getByteOrder ValString{}      = Nothing
getByteOrder ValFixedString{} = Nothing
getByteOrder ValOctet{}       = Nothing
getByteOrder ValFixedOctet{}  = Nothing
getByteOrder (ValCUCTime _)   = Just BiE
getByteOrder ValUndefined     = Nothing



{-# INLINABLE isStorableWord64 #-}
-- | Returns, if a Value can be converted into a 'Word64' with the 'getInt'
-- function. This means, it is not necessarily an integer value (e.g. ValDouble
-- can also be converted to Word64). Used internally for encoding a packet
isStorableWord64 :: Value -> Bool
isStorableWord64 ValInt8{}    = True
isStorableWord64 ValInt16{}   = True
isStorableWord64 ValInt24{}   = True
isStorableWord64 ValInt32{}   = True
isStorableWord64 ValInt64{}   = True
isStorableWord64 ValUInt8X{}  = True
isStorableWord64 ValUInt8{}   = True
isStorableWord64 ValUInt16X{} = True
isStorableWord64 ValUInt16{}  = True
isStorableWord64 ValUInt24{}  = True
isStorableWord64 ValUInt32{}  = True
isStorableWord64 ValUInt64{}  = True
isStorableWord64 ValDouble{}  = True
isStorableWord64 _            = False



instance Display Value where
    display (ValInt8 x     ) = display x
    display (ValInt16  _ x ) = display x
    display (ValInt24  _ x ) = display x
    display (ValInt32  _ x ) = display x
    display (ValInt64  _ x ) = display x
    display (ValUInt8X _ x ) = display x
    display (ValUInt8 x    ) = display x
    display (ValUInt16X _ x) = display x
    display (ValUInt16  _ x) = display x
    display (ValUInt32X _ x) = display x
    display (ValUInt24  _ x) = display x
    display (ValUInt32  _ x) = display x
    display (ValUInt64  _ x) = display x
    display (ValDouble  _ x) = display x
    display (ValString x   ) = displayBytesUtf8 x
    display (ValFixedString n x) =
        displayBytesUtf8 $ leftPaddedC ' ' (fromIntegral n) x
    display (ValOctet x       ) = display $ hexdumpLineBS x
    display (ValFixedOctet _ x) = display $ hexdumpLineBS x
    display (ValCUCTime x     ) = display x
    display ValUndefined        = display ("UNDEFINED" :: Text)


class Integral a => GetInt a where
    getInt :: Value -> a


instance GetInt Word64 where
    {-# INLINABLE getInt #-}
    getInt (ValInt8 x     ) = fromIntegral x
    getInt (ValInt16  _ x ) = fromIntegral x
    getInt (ValInt24  _ x ) = fromIntegral x
    getInt (ValInt32  _ x ) = fromIntegral x
    getInt (ValInt64  _ x ) = fromIntegral x
    getInt (ValUInt8X _ x ) = fromIntegral x
    getInt (ValUInt8 x    ) = fromIntegral x
    getInt (ValUInt16X _ x) = fromIntegral x
    getInt (ValUInt16  _ x) = fromIntegral x
    getInt (ValUInt32X _ x) = fromIntegral x
    getInt (ValUInt24  _ x) = fromIntegral x
    getInt (ValUInt32  _ x) = fromIntegral x
    getInt (ValUInt64  _ x) = x
    getInt (ValDouble  _ x) = round x
    getInt (ValString x   ) = case parseOnly decimal x of
        Left  _   -> 0
        Right val -> val
    getInt (ValFixedString _ x) = case parseOnly decimal x of
        Left  _   -> 0
        Right val -> val
    getInt (ValOctet x) = case parseOnly anyWord64be x of
        Left  _   -> 0
        Right val -> val
    getInt (ValFixedOctet _ x) = case parseOnly anyWord64be x of
        Left  _   -> 0
        Right val -> val
    getInt (ValCUCTime x) = fromIntegral $ timeToMicro x
    getInt ValUndefined   = 0

instance GetInt Int64 where
    {-# INLINABLE getInt #-}
    getInt (ValInt8 x     ) = fromIntegral x
    getInt (ValInt16  _ x ) = fromIntegral x
    getInt (ValInt24  _ x ) = fromIntegral x
    getInt (ValInt32  _ x ) = fromIntegral x
    getInt (ValInt64  _ x ) = x
    getInt (ValUInt8X _ x ) = fromIntegral x
    getInt (ValUInt8 x    ) = fromIntegral x
    getInt (ValUInt16X _ x) = fromIntegral x
    getInt (ValUInt16  _ x) = fromIntegral x
    getInt (ValUInt32X _ x) = fromIntegral x
    getInt (ValUInt24  _ x) = fromIntegral x
    getInt (ValUInt32  _ x) = fromIntegral x
    getInt (ValUInt64  _ x) = fromIntegral x
    getInt (ValDouble  _ x) = round x
    getInt (ValString x   ) = case parseOnly decimal x of
        Left  _   -> 0
        Right val -> val
    getInt (ValFixedString _ x) = case parseOnly decimal x of
        Left  _   -> 0
        Right val -> val
    getInt (ValOctet x) = case parseOnly anyWord64be x of
        Left  _   -> 0
        Right val -> fromIntegral val
    getInt (ValFixedOctet _ x) = case parseOnly anyWord64be x of
        Left  _   -> 0
        Right val -> fromIntegral val
    getInt (ValCUCTime x) = timeToMicro x
    getInt ValUndefined   = 0



instance GetInt Integer where
    {-# INLINABLE getInt #-}
    getInt (ValInt8 x     ) = fromIntegral x
    getInt (ValInt16  _ x ) = fromIntegral x
    getInt (ValInt24  _ x ) = fromIntegral x
    getInt (ValInt32  _ x ) = fromIntegral x
    getInt (ValInt64  _ x ) = fromIntegral x
    getInt (ValUInt8X _ x ) = fromIntegral x
    getInt (ValUInt8 x    ) = fromIntegral x
    getInt (ValUInt16X _ x) = fromIntegral x
    getInt (ValUInt16  _ x) = fromIntegral x
    getInt (ValUInt32X _ x) = fromIntegral x
    getInt (ValUInt24  _ x) = fromIntegral x
    getInt (ValUInt32  _ x) = fromIntegral x
    getInt (ValUInt64  _ x) = fromIntegral x
    getInt (ValDouble  _ x) = round x
    getInt (ValString x   ) = case parseOnly decimal x of
        Left  _   -> 0
        Right val -> val
    getInt (ValFixedString _ x) = case parseOnly decimal x of
        Left  _   -> 0
        Right val -> val
    getInt (ValOctet x       ) = fromBytesToInteger BiE x
    getInt (ValFixedOctet _ x) = fromBytesToInteger BiE x
    getInt (ValCUCTime x     ) = fromIntegral (timeToMicro x)
    getInt ValUndefined        = 0



{-# INLINABLE table8 #-}
table8 :: Vector Word8
table8 = V.map (\x -> (1 `shiftL` x) - 1) (V.fromList [0 .. 7])

{-# INLINABLE table16 #-}
table16 :: Vector Word16
table16 = V.map (\x -> (1 `shiftL` x) - 1) (V.fromList [0 .. 15])

{-# INLINABLE table32 #-}
table32 :: Vector Word32
table32 = V.map (\x -> (1 `shiftL` x) - 1) (V.fromList [0 .. 31])



class Integral a => SetInt a where
    setInt :: Value -> a -> Value

instance SetInt Integer where
    {-# INLINABLE setInt #-}
    setInt (ValInt8 _   ) x = ValInt8 (fromIntegral x)
    setInt (ValInt16 b _) x = ValInt16 b (fromIntegral x)
    setInt (ValInt24 b _) x = ValInt24 b (fromIntegral x)
    setInt (ValInt32 b _) x = ValInt32 b (fromIntegral x)
    setInt (ValInt64 b _) x = ValInt64 b (fromIntegral x)
    setInt (ValUInt8X w _) x =
        ValUInt8X w (fromIntegral x .&. table8 V.! fromIntegral (unB8 w))
    setInt (ValUInt8 _) x = ValUInt8 (fromIntegral x)
    setInt (ValUInt16X w _) x =
        ValUInt16X w (fromIntegral x .&. table16 V.! fromIntegral (unB16 w))
    setInt (ValUInt16 b _) x = ValUInt16 b (fromIntegral x)
    setInt (ValUInt32X w _) x =
        ValUInt32X w (fromIntegral x .&. table32 V.! fromIntegral (unB32 w))
    setInt (ValUInt24 b _         ) x = ValUInt24 b (fromIntegral x)
    setInt (ValUInt32 b _         ) x = ValUInt32 b (fromIntegral x)
    setInt (ValUInt64 b _         ) x = ValUInt64 b (fromIntegral x)
    setInt (ValDouble b _         ) x = ValDouble b (fromIntegral x)
    setInt (ValString _) x = ValString (builderBytes (asciiIntegral x))
    setInt (ValFixedString width _) x = ValFixedString width $ rightPaddedC
        ' '
        (fromIntegral width)
        (builderBytes (asciiIntegral x))
    setInt (ValOctet _) x = ValOctet (toBytes x)
    setInt (ValFixedOctet width _) x =
        ValOctet (rightPadded 0 (fromIntegral width) (toBytes x))
    setInt (ValCUCTime _) x = ValCUCTime (microToTime (fromIntegral x) False)
    setInt ValUndefined   _ = ValUndefined

instance SetInt Word64 where
    {-# INLINABLE setInt #-}
    setInt (ValInt8 _    ) x = ValInt8 (fromIntegral (x .&. 0xFF))
    setInt (ValInt16  b _) x = ValInt16 b (fromIntegral (x .&. 0xFFFF))
    setInt (ValInt24  b _) x = ValInt24 b (fromIntegral (x .&. 0xFFFFFF))
    setInt (ValInt32  b _) x = ValInt32 b (fromIntegral (x .&. 0xFFFFFFFF))
    setInt (ValInt64  b _) x = ValInt64 b (fromIntegral x)
    setInt (ValUInt8X w _) x = ValUInt8X
        w
        (fromIntegral (x .&. fromIntegral (table8 V.! fromIntegral (unB8 w))))
    setInt (ValUInt8 _            ) x = ValUInt8 (fromIntegral x)
    setInt (ValUInt16X w _        ) x = ValUInt16X w (fromIntegral x)
    setInt (ValUInt16  b _        ) x = ValUInt16 b (fromIntegral x)
    setInt (ValUInt32X w _        ) x = ValUInt32X w (fromIntegral x)
    setInt (ValUInt24  b _        ) x = ValUInt32 b (fromIntegral x)
    setInt (ValUInt32  b _        ) x = ValUInt32 b (fromIntegral x)
    setInt (ValUInt64  b _        ) x = ValUInt64 b x
    setInt (ValDouble  b _        ) x = ValDouble b (fromIntegral x)
    setInt (ValString _) x = ValString (builderBytes (asciiIntegral x))
    setInt (ValFixedString width _) x = ValFixedString width $ rightPaddedC
        ' '
        (fromIntegral width)
        (builderBytes (asciiIntegral x))
    setInt (ValOctet _           ) x = ValOctet (builderBytes (word64BE x))
    setInt (ValFixedOctet width _) x = ValFixedOctet width
        $ rightPadded 0 (fromIntegral width) (builderBytes (word64BE x))
    setInt (ValCUCTime _) x = ValCUCTime (microToTime (fromIntegral x) False)
    setInt ValUndefined   _ = ValUndefined


{-# INLINABLE toBytes #-}
toBytes :: Integer -> ByteString
toBytes x = B.reverse . B.unfoldr (fmap go) . Just $ changeSign x
  where
    changeSign :: Num a => a -> a
    changeSign | x < 0     = subtract 1 . negate
               | otherwise = id
    go :: Integer -> (Word8, Maybe Integer)
    go x' = (b, i)
      where
        b = changeSign (fromInteger x')
        i | x' >= 128 = Just (x' `shiftR` 8)
          | otherwise = Nothing

{-# INLINABLE fromBytesToInteger #-}
fromBytesToInteger :: Endian -> ByteString -> Integer
fromBytesToInteger BiE = B.foldl' f 0
    where f a b = a `shiftL` 8 .|. fromIntegral b
fromBytesToInteger LiE = fromBytesToInteger BiE . B.reverse



{-# INLINABLE isSetableAligned #-}
isSetableAligned :: Value -> Bool
isSetableAligned ValUInt8{}       = True
isSetableAligned ValUInt16{}      = True
isSetableAligned ValUInt32{}      = True
isSetableAligned ValUInt64{}      = True
isSetableAligned ValInt8{}        = True
isSetableAligned ValInt16{}       = True
isSetableAligned ValInt32{}       = True
isSetableAligned ValInt64{}       = True
isSetableAligned ValDouble{}      = True
isSetableAligned ValString{}      = True
isSetableAligned ValFixedString{} = True
isSetableAligned ValOctet{}       = True
isSetableAligned ValFixedOctet{}  = True
isSetableAligned ValCUCTime{}     = True
isSetableAligned _                = False


{-# INLINABLE setAlignedValue #-}
setAlignedValue :: VS.MVector s Word8 -> ByteOffset -> Value -> ST s ()
setAlignedValue vec !off (ValUInt8 x   ) = setValue vec off BiE x
setAlignedValue vec !off (ValUInt16 b x) = setValue vec off b x
setAlignedValue vec !off (ValUInt32 b x) = setValue vec off b x
setAlignedValue vec !off (ValUInt64 b x) = setValue vec off b x
setAlignedValue vec !off (ValInt8 x    ) = setValue vec off BiE x
setAlignedValue vec !off (ValInt16  b x) = setValue vec off b x
setAlignedValue vec !off (ValInt32  b x) = setValue vec off b x
setAlignedValue vec !off (ValInt64  b x) = setValue vec off b x
setAlignedValue vec !off (ValDouble b x) = setValue vec off b x
setAlignedValue vec !off (ValString x  ) = copyBS vec off x
setAlignedValue vec !off (ValFixedString width x) =
    copyBS vec off (rightPaddedC ' ' (fromIntegral width) x)
setAlignedValue vec !off (ValOctet x) = copyBS vec off x
setAlignedValue vec !off (ValFixedOctet width x) =
    copyBS vec off (rightPadded 0 (fromIntegral width) x)
setAlignedValue vec !off (ValCUCTime x) = setValue vec off BiE x
setAlignedValue _   _    _              = pure ()


{-# INLINABLE getAlignedValue #-}
getAlignedValue :: ByteString -> ByteOffset -> Value -> Maybe Value
getAlignedValue byts off ValUInt8{}      = ValUInt8 <$> getValue byts off BiE
getAlignedValue byts off (ValUInt16 b _) = ValUInt16 b <$> getValue byts off b
getAlignedValue byts off (ValUInt32 b _) = ValUInt32 b <$> getValue byts off b
getAlignedValue byts off (ValUInt64 b _) = ValUInt64 b <$> getValue byts off b
getAlignedValue byts off ValInt8{}       = ValInt8 <$> getValue byts off BiE
getAlignedValue byts off (ValInt16  b _) = ValInt16 b <$> getValue byts off b
getAlignedValue byts off (ValInt32  b _) = ValInt32 b <$> getValue byts off b
getAlignedValue byts off (ValInt64  b _) = ValInt64 b <$> getValue byts off b
getAlignedValue byts off (ValDouble b _) = ValDouble b <$> getValue byts off b
getAlignedValue byts off (ValString _  ) = ValString <$> getValueOctet byts off
getAlignedValue byts off (ValFixedString len _) =
    ValFixedString len <$> getValueOctetLen byts off (fromIntegral len)
getAlignedValue byts off (ValOctet _) = ValOctet <$> getValueOctet byts off
getAlignedValue byts off (ValFixedOctet len _) =
    ValFixedOctet len <$> getValueOctetLen byts off (fromIntegral len)
getAlignedValue byts off (ValCUCTime (CUCTime _ _ delta)) =
    ValCUCTime . cucTimeSetDelta delta <$> getValue byts off BiE
getAlignedValue _ _ _ = Just ValUndefined


{-# INLINABLE getUnalignedValue #-}
getUnalignedValue :: ByteString -> Offset -> Value -> Maybe Value
getUnalignedValue byts off ValUInt8{} =
    ValUInt8 . fromIntegral <$> getBitFieldInt64 byts off (BitSize 8) BiE
getUnalignedValue byts off (ValUInt16 b _) =
    ValUInt16 b . fromIntegral <$> getBitFieldInt64 byts off (BitSize 16) b
getUnalignedValue byts off (ValUInt32 b _) =
    ValUInt32 b . fromIntegral <$> getBitFieldInt64 byts off (BitSize 32) b
getUnalignedValue byts off (ValUInt64 b _) =
    ValUInt64 b . fromIntegral <$> getBitFieldInt64 byts off (BitSize 64) b
getUnalignedValue byts off (ValUInt8X w _) =
    ValUInt8X w
        .   fromIntegral
        <$> getBitFieldWord64 byts off (BitSize (fromIntegral (unB8 w))) BiE
getUnalignedValue byts off ValInt8{} =
    ValInt8 . fromIntegral <$> getBitFieldInt64 byts off (BitSize 8) BiE
getUnalignedValue byts off (ValInt16 b _) =
    ValInt16 b . fromIntegral <$> getBitFieldInt64 byts off (BitSize 16) b
getUnalignedValue byts off (ValInt32 b _) =
    ValInt32 b . fromIntegral <$> getBitFieldInt64 byts off (BitSize 32) b
getUnalignedValue byts off (ValInt64 b _) =
    ValInt64 b <$> getBitFieldInt64 byts off (BitSize 64) b
getUnalignedValue byts off (ValDouble b _) =
    ValDouble b <$> getBitFieldDouble byts off b
getUnalignedValue _ _ _ = Just ValUndefined

{-# INLINABLE isGettableUnaligned #-}
isGettableUnaligned :: Value -> Bool
isGettableUnaligned ValUInt8X{} = True
isGettableUnaligned ValUInt8{}  = True
isGettableUnaligned ValUInt16{} = True
isGettableUnaligned ValUInt32{} = True
isGettableUnaligned ValUInt64{} = True
isGettableUnaligned ValInt8{}   = True
isGettableUnaligned ValInt16{}  = True
isGettableUnaligned ValInt32{}  = True
isGettableUnaligned ValInt64{}  = True
isGettableUnaligned _           = False

--     | ValOctet !ByteString
--     | ValFixedOctet !Word16 !ByteString
--     | ValCUCTime CUCTime
--     | ValUndefined


typeColumn :: Int
typeColumn = 12

endianColumn :: Int
endianColumn = 3

val1Column :: Int
val1Column = 21


emptyEndian :: TB.Builder 
emptyEndian = foldMap TB.char (replicate endianColumn ' ')


valueHeaderLine :: TB.Builder 
valueHeaderLine = 
    let len = typeColumn + endianColumn + val1Column
    in
    padFromRight typeColumn ' ' (TB.text "Type")
        <> padFromRight endianColumn ' ' (TB.text "BO")
        <> padFromRight val1Column   ' ' (TB.text "Value")

buildEndianUnsigned :: Integral a => Text -> Endian -> a -> TB.Builder 
buildEndianUnsigned typ b x = 
    padFromRight typeColumn ' ' (TB.text typ)
        <> padFromRight endianColumn ' ' (endianBuilder b)
        <> padFromRight val1Column   ' ' (TB.unsignedDecimal x)
        <> TB.text " (0x"
        <> TB.hexadecimal x
        <> TB.text ")"

builderUnsigned :: Integral a => Word8 -> a -> TB.Builder 
builderUnsigned width x = 
    padFromRight typeColumn ' ' (TB.text "UINT" <> TB.decimal width)
        <> emptyEndian
        <> padFromRight val1Column   ' ' (TB.unsignedDecimal x)
        <> TB.text " (0x"
        <> TB.hexadecimal x
        <> TB.text ")"

builderSigned :: Integral a => Word8 -> a -> TB.Builder 
builderSigned width x = 
    padFromRight typeColumn ' ' (TB.text "INT" <> TB.decimal width)
        <> emptyEndian
        <> padFromRight val1Column   ' ' (TB.decimal x)


buildEndianSigned :: Integral a => Text -> Endian -> a -> TB.Builder 
buildEndianSigned typ b x = 
    padFromRight typeColumn ' ' (TB.text typ)
        <> padFromRight endianColumn ' ' (endianBuilder b)
        <> padFromRight val1Column   ' ' (TB.decimal x)


builderReal :: Text -> Endian -> Double -> TB.Builder 
builderReal typ b x = 
    padFromRight typeColumn ' ' (TB.text typ)
        <> padFromRight endianColumn ' ' (endianBuilder b)
        <> TB.fixedDouble 16 x

octetBuilder x = 
    let chunks = chunkedByBS 4 x 
        
        convChunk = mconcat . map byteToHex . B.unpack
        converted = map convChunk chunks 

        values = chunksIntersperse 2 ["\n"] converted

        byteToHex :: Word8 -> TB.Builder 
        byteToHex x = TB.padFromLeft 2 '0' (TB.hexadecimal x)
    in
    mconcat (mconcat values)



valueBuilder :: Value -> TB.Builder
valueBuilder (ValUInt16 b x) = buildEndianUnsigned "UINT16" b x 
valueBuilder (ValUInt24 b x) = buildEndianUnsigned "UINT24" b x 
valueBuilder (ValUInt32 b x) = buildEndianUnsigned "UINT32" b x 
valueBuilder (ValUInt64 b x) = buildEndianUnsigned "UINT64" b x 

valueBuilder (ValInt16 b x) = buildEndianSigned "INT16" b x 
valueBuilder (ValInt24 b x) = buildEndianSigned "INT24" b x 
valueBuilder (ValInt32 b x) = buildEndianSigned "INT32" b x 
valueBuilder (ValInt64 b x) = buildEndianSigned "INT64" b x 

valueBuilder (ValUInt8X w x) = builderUnsigned (unB8 w) x 
valueBuilder (ValUInt8 x) = builderUnsigned 8 x 
valueBuilder (ValUInt16X w x) = builderUnsigned (unB16 w) x 
valueBuilder (ValUInt32X w x) = builderUnsigned (unB32 w) x 
valueBuilder (ValInt8 x) = builderSigned 8 x

valueBuilder (ValDouble b x) = builderReal "DOUBLE" b x 

valueBuilder (ValString x) = 
    padFromRight typeColumn ' ' (TB.text "STRING")
        <> emptyEndian
        <> TB.asciiByteString x

valueBuilder (ValFixedString l x) = 
    padFromRight typeColumn ' ' (TB.text "STRING" <> TB.decimal l)
        <> emptyEndian
        <> TB.asciiByteString x

valueBuilder (ValOctet x) = 
    padFromRight typeColumn ' ' (TB.text "STRING")
        <> emptyEndian
        <> octetBuilder x 

valueBuilder (ValFixedOctet l x) = 
    padFromRight typeColumn ' ' (TB.text "STRING" <> TB.decimal l)
        <> emptyEndian
        <> octetBuilder x

valueBuilder (ValCUCTime x) = 
    padFromRight typeColumn ' ' (TB.text "CUC_4_2")
        <> emptyEndian
        <> TB.text (textDisplay x)

valueBuilder ValUndefined = 
    TB.text "UNDEFINED"


