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
    , ScopedTypeVariables
    , DataKinds
    , DeriveAnyClass
#-}
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
    ) where

import           RIO
import qualified RIO.ByteString                as B
import qualified RIO.Text                      as T
import qualified RIO.HashMap                   as HM
import qualified RIO.Vector.Storable           as VS

import qualified Data.ByteString.Char8         as BC
import           Data.Bits
import           Data.Attoparsec.ByteString    as A
import           Data.Attoparsec.Binary        as A
import           Data.Binary
import           Data.Aeson                    as AE
                                         hiding ( Value )
import           Data.ByteString.Base64.Type
import           Data.Attoparsec.ByteString.Char8
                                               as A
import           ByteString.StrictBuilder

import           Codec.Serialise

import           Data.PUS.EncTime

import           General.SizeOf

import           General.Padding
import           General.Hexdump
import           General.Types
import           General.PUSTypes
import           General.Time
import           General.SetBitField
import           General.GetBitField



data Value =
    ValInt8 !Int8
    | ValInt16 Endian !Word16
    | ValUInt3 !Word8
    | ValUInt8 !Word8
    | ValUInt16 Endian !Word16
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

instance Binary Value
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
    toJSON (ValUInt3 x) =
        object ["valType" .= ("ValUInt3" :: Text), "value" .= x]
    toJSON (ValUInt8 x) =
        object ["valType" .= ("ValUInt8" :: Text), "value" .= x]
    toJSON (ValUInt16 e x) =
        object ["valType" .= ("ValUInt16" :: Text), "value" .= x, "endian" .= e]
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
    toEncoding (ValUInt3 x) =
        pairs ("valType" .= ("ValUInt3" :: Text) <> "value" .= x)
    toEncoding (ValUInt8 x) =
        pairs ("valType" .= ("ValUInt8" :: Text) <> "value" .= x)
    toEncoding (ValUInt16 e x) = pairs
        (  "valType"
        .= ("ValUInt16" :: Text)
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
    parseJSON (Object o) = case HM.lookup "valType" o of
        Just (String "ValInt8" ) -> ValInt8 <$> o .: "value"
        Just (String "ValInt16") -> ValInt16 <$> o .: "endian" <*> o .: "value"
        Just (String "ValUInt3") -> ValUInt3 <$> o .: "value"
        Just (String "ValUInt8") -> ValUInt8 <$> o .: "value"
        Just (String "ValUInt16") ->
            ValUInt16 <$> o .: "endian" <*> o .: "value"
        Just (String "ValUInt32") ->
            ValUInt32 <$> o .: "endian" <*> o .: "value"
        Just (String "ValUInt64") ->
            ValUInt64 <$> o .: "endian" <*> o .: "value"
        Just (String "ValDouble") ->
            ValDouble <$> o .: "endian" <*> o .: "value"
        Just (String "ValString") -> ValString . encodeUtf8 <$> o .: "value"
        Just (String "ValFixedString") ->
            ValFixedString <$> o .: "width" <*> (encodeUtf8 <$> o .: "value")
        Just (String "ValOctet") -> ValOctet . getByteString64 <$> o .: "value"
        Just (String "ValFixedOctet") ->
            ValFixedOctet
                <$> o
                .:  "width"
                <*> (getByteString64 <$> o .: "value")
        Just (String "ValCUCTime"  ) -> ValCUCTime <$> o .: "value"
        Just (String "ValUndefined") -> pure ValUndefined
        _                            -> pure ValUndefined
    parseJSON _ = pure ValUndefined



{-# INLINABLE initialValue #-}
initialValue :: Endian -> PTC -> PFC -> Value
initialValue _ (PTC 3) (PFC 4 ) = ValUInt8 0
initialValue b (PTC 3) (PFC 12) = ValUInt16 b 0
initialValue b (PTC 3) (PFC 14) = ValUInt32 b 0
initialValue b (PTC 3) (PFC 16) = ValUInt64 b 0
initialValue _ (PTC 4) (PFC 4 ) = ValInt8 0
initialValue b (PTC 4) (PFC 12) = ValInt16 b 0
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
    bitSize ValInt8{}     = mkBitSize 8
    bitSize ValInt16{}    = mkBitSize 16
    bitSize ValUInt3{}    = mkBitSize 3
    bitSize ValUInt8{}    = mkBitSize 8
    bitSize ValUInt16{}   = mkBitSize 16
    bitSize ValUInt32{}   = mkBitSize 32
    bitSize ValUInt64{}   = mkBitSize 64
    bitSize ValDouble{}   = mkBitSize 64
    bitSize (ValString x) = bytesToBitSize . mkByteSize $ B.length x
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
getByteOrder (ValInt16 b _ )  = Just b
getByteOrder (ValUInt3 _   )  = Nothing
getByteOrder (ValUInt8 _   )  = Nothing
getByteOrder (ValUInt16 b _)  = Just b
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
isStorableWord64 ValInt8{}   = True
isStorableWord64 ValInt16{}  = True
isStorableWord64 ValUInt3{}  = True
isStorableWord64 ValUInt8{}  = True
isStorableWord64 ValUInt16{} = True
isStorableWord64 ValUInt32{} = True
isStorableWord64 ValUInt64{} = True
isStorableWord64 ValDouble{} = True
isStorableWord64 _           = False



instance Display Value where
    display (ValInt8 x    ) = display x
    display (ValInt16 _ x ) = display x
    display (ValUInt3 x   ) = display x
    display (ValUInt8 x   ) = display x
    display (ValUInt16 _ x) = display x
    display (ValUInt32 _ x) = display x
    display (ValUInt64 _ x) = display x
    display (ValDouble _ x) = display x
    display (ValString x  ) = displayBytesUtf8 x
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
    getInt (ValInt8 x    ) = fromIntegral x
    getInt (ValInt16 _ x ) = fromIntegral x
    getInt (ValUInt3 x   ) = fromIntegral x
    getInt (ValUInt8 x   ) = fromIntegral x
    getInt (ValUInt16 _ x) = fromIntegral x
    getInt (ValUInt32 _ x) = fromIntegral x
    getInt (ValUInt64 _ x) = x
    getInt (ValDouble _ x) = round x
    getInt (ValString x  ) = case parseOnly decimal x of
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
    getInt (ValInt8 x    ) = fromIntegral x
    getInt (ValInt16 _ x ) = fromIntegral x
    getInt (ValUInt3 x   ) = fromIntegral x
    getInt (ValUInt8 x   ) = fromIntegral x
    getInt (ValUInt16 _ x) = fromIntegral x
    getInt (ValUInt32 _ x) = fromIntegral x
    getInt (ValUInt64 _ x) = fromIntegral x
    getInt (ValDouble _ x) = round x
    getInt (ValString x  ) = case parseOnly decimal x of
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
    getInt (ValInt8 x    ) = fromIntegral x
    getInt (ValInt16 _ x ) = fromIntegral x
    getInt (ValUInt3 x   ) = fromIntegral x
    getInt (ValUInt8 x   ) = fromIntegral x
    getInt (ValUInt16 _ x) = fromIntegral x
    getInt (ValUInt32 _ x) = fromIntegral x
    getInt (ValUInt64 _ x) = fromIntegral x
    getInt (ValDouble _ x) = round x
    getInt (ValString x  ) = case parseOnly decimal x of
        Left  _   -> 0
        Right val -> val
    getInt (ValFixedString _ x) = case parseOnly decimal x of
        Left  _   -> 0
        Right val -> val
    getInt (ValOctet x       ) = fromBytesToInteger BiE x
    getInt (ValFixedOctet _ x) = fromBytesToInteger BiE x
    getInt (ValCUCTime x     ) = fromIntegral (timeToMicro x)
    getInt ValUndefined        = 0


class Integral a => SetInt a where
    setInt :: Value -> a -> Value

instance SetInt Integer where
    {-# INLINABLE setInt #-}
    setInt (ValInt8 _             ) x = ValInt8 (fromIntegral x)
    setInt (ValInt16 b _          ) x = ValInt16 b (fromIntegral x)
    setInt (ValUInt3 _            ) x = ValUInt3 (fromIntegral x .&. 0x07)
    setInt (ValUInt8 _            ) x = ValUInt8 (fromIntegral x)
    setInt (ValUInt16 b _         ) x = ValUInt16 b (fromIntegral x)
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
    setInt (ValInt8 _             ) x = ValInt8 (fromIntegral (x .&. 0xFF))
    setInt (ValInt16 b _          ) x = ValInt16 b (fromIntegral (x .&. 0xFFFF))
    setInt (ValUInt3 _            ) x = ValUInt3 (fromIntegral (x .&. 0x07))
    setInt (ValUInt8 _            ) x = ValUInt8 (fromIntegral x)
    setInt (ValUInt16 b _         ) x = ValUInt16 b (fromIntegral x)
    setInt (ValUInt32 b _         ) x = ValUInt32 b (fromIntegral x)
    setInt (ValUInt64 b _         ) x = ValUInt64 b x
    setInt (ValDouble b _         ) x = ValDouble b (fromIntegral x)
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
getUnalignedValue byts off ValUInt3{} =
    ValUInt3 . fromIntegral <$> getBitFieldWord64 byts off (BitSize 3) BiE
getUnalignedValue byts off ValInt8{} =
    ValInt8 . fromIntegral <$> getBitFieldInt64 byts off (BitSize 8) BiE
getUnalignedValue byts off (ValInt16 b _) =
    ValInt16 b . fromIntegral <$> getBitFieldInt64 byts off (BitSize 16) b
getUnalignedValue byts off (ValDouble b _) =
    ValDouble b <$> getBitFieldDouble byts off b
getUnalignedValue _ _ _ = Just ValUndefined

{-# INLINABLE isGettableUnaligned #-}
isGettableUnaligned :: Value -> Bool
isGettableUnaligned ValUInt3{}  = True
isGettableUnaligned ValUInt8{}   = True
isGettableUnaligned ValUInt16{}   = True
isGettableUnaligned ValUInt32{}   = True
isGettableUnaligned ValUInt64{}   = True
isGettableUnaligned ValInt8{}   = True
isGettableUnaligned ValInt16{}  = True
isGettableUnaligned _           = False
