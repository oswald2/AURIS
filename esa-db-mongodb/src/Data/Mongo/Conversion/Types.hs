{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Mongo.Conversion.Types where

import           RIO                     hiding ( lookup )

import           Data.Bson

import           General.Types
import           General.PUSTypes
import           General.APID
import           Data.PUS.SegmentationFlags
import           Data.Mongo.Conversion.Class    ( MongoDbConversion(..) )



instance Val (Flag Good) where
    val flag = val (toBool flag)

    cast' (Bool x) = Just (toFlag Good x)
    cast' _        = Nothing


instance Val SCID where
    val (SCID x) = Int32 (fromIntegral x)

    cast' (Int32 x) = Just $ SCID (fromIntegral x)
    cast' _         = Nothing

instance Val VCID where
    val (VCID x) = Int32 (fromIntegral x)

    cast' (Int32 x) = Just $ VCID (fromIntegral x)
    cast' _         = Nothing

instance Val SSC where
    val ssc = val (getSSC ssc)

    cast' (Int32 x) = Just $ mkSSC (fromIntegral x)
    cast' _         = Nothing


instance Val Word8 where
    val x = Int32 (fromIntegral x)

    cast' (Int32 x) = Just (fromIntegral x)
    cast' _         = Nothing

instance Val Word16 where
    val x = Int32 (fromIntegral x)

    cast' (Int32 x) = Just (fromIntegral x)
    cast' _         = Nothing

instance Val Word32 where
    val x = Int32 (fromIntegral x)

    cast' (Int32 x) = Just (fromIntegral x)
    cast' _         = Nothing


instance Val ByteString  where
    val x = Bin (Binary x)

    cast' (Bin (Binary x)) = Just x
    cast' _                = Nothing



instance MongoDbConversion EduVCID Document where
    toDB IsSCOE     = ["VCID" =: String "IsSCOE"]
    toDB (IsVCID x) = ["VCID" =: x]

    fromDB doc = do
        v <- lookup "VCID" doc
        case v of
            String "IsSCOE" -> Just IsSCOE
            Int32  vcid     -> Just (IsVCID (VCID (fromIntegral vcid)))
            _               -> Nothing


instance Val SegmentationFlags where
    val SegmentStandalone = String "UNSEG"
    val SegmentFirst      = String "FIRST"
    val SegmentContinue   = String "CONT"
    val SegmentLast       = String "LAST"

    cast' (String "UNSEG") = Just SegmentStandalone
    cast' (String "FIRST") = Just SegmentFirst
    cast' (String "CONT" ) = Just SegmentContinue
    cast' (String "LAST" ) = Just SegmentLast
    cast' _                = Nothing


instance Val APID where
    val (APID x) = val x

    cast' (Int32 x) = Just (APID (fromIntegral x))
    cast' _         = Nothing


instance Val PUSPacketType where
    val PUSTM = String "TM"
    val PUSTC = String "TC"

    cast' (String "TM") = Just PUSTM
    cast' (String "TC") = Just PUSTC
    cast' _             = Nothing


instance Val PUSType  where
    val (PUSType x) = val x

    cast' (Int32 x) = Just (PUSType (fromIntegral x))
    cast' _         = Nothing


instance Val PUSSubType  where
    val (PUSSubType x) = val x

    cast' (Int32 x) = Just (PUSSubType (fromIntegral x))
    cast' _         = Nothing


instance Val SourceID  where
    val (SourceID x) = val x

    cast' (Int32 x) = Just (SourceID (fromIntegral x))
    cast' _         = Nothing



instance Val ByteOffset  where
    val (ByteOffset x) = val x

    cast' (Int32 x) = Just (ByteOffset (fromIntegral x))
    cast' _         = Nothing

instance Val BitSize  where
    val (BitSize x) = val x

    cast' (Int32 x) = Just (BitSize (fromIntegral x))
    cast' _         = Nothing
