{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Mongo.Conversion.Types where

import           Data.Bson
import           Data.Text.Short               as ST
import           RIO                     hiding ( lookup )

import           General.APID
import           General.PUSTypes
import           General.Time
import           General.Types

import           Data.PUS.SegmentationFlags
import           Data.TM.Validity
import           Data.TM.Value                 as TM

import           Protocol.ProtocolInterfaces


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


instance Val HexBytes where
    val (HexBytes x) = Bin (Binary x)

    cast' (Bin (Binary x)) = Just (HexBytes x)
    cast' _                = Nothing


instance Val EduVCID where
    val IsSCOE     = Doc ["VCID" =: String "IsSCOE"]
    val (IsVCID x) = Doc ["VCID" =: x]

    cast' (Doc doc) = do
        v <- lookup "VCID" doc
        case v of
            String "IsSCOE" -> Just IsSCOE
            Int32  vcid     -> Just (IsVCID (VCID (fromIntegral vcid)))
            _               -> Nothing
    cast' _ = Nothing

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


instance Val SourceIDC  where
    val (SourceIDC x) = val x

    cast' (Int32 x) = Just (SourceIDC (fromIntegral x))
    cast' _         = Nothing



instance Val ByteOffset  where
    val (ByteOffset x) = val x

    cast' (Int32 x) = Just (ByteOffset (fromIntegral x))
    cast' _         = Nothing

instance Val BitSize  where
    val (BitSize x) = val x

    cast' (Int32 x) = Just (BitSize (fromIntegral x))
    cast' _         = Nothing


instance Val SunTime where
    val x = Doc ["value" =: Int64 (timeToMicro x), "delta" =: Bool (isDelta x)]

    cast' (Doc doc) = do
        v <- lookup "value" doc
        d <- lookup "delta" doc
        return (microToTime v d)
    cast' _ = Nothing

instance Val ShortText where
    val x = String (ST.toText x)

    cast' (String x) = Just (ST.fromText x)
    cast' _          = Nothing


instance Val TMValueSimple where
    val (TMValInt x) = Doc ["type" =: String "Int", "value" =: Int64 x]
    val (TMValUInt x) =
        Doc ["type" =: String "UInt", "value" =: Int64 (fromIntegral x)]
    val (TMValDouble x) = Doc ["type" =: String "Double", "value" =: Float x]
    val (TMValTime   x) = Doc ["type" =: String "Time", "value" =: val x]
    val (TMValString x) = Doc ["type" =: String "String", "value" =: val x]
    val (TMValOctet x) =
        Doc ["type" =: String "Octet", "value" =: Bin (Binary (toBS x))]
    val TMValNothing = Doc ["type" =: String "Nothing", "value" =: Null]

    cast' (Doc doc) = do
        t <- lookup "type" doc
        case t of
            String "Int" -> do
                v <- lookup "value" doc
                return (TMValInt v)
            String "UInt" -> do
                v :: Int64 <- lookup "value" doc
                return (TMValUInt (fromIntegral v))
            String "Double" -> do
                v <- lookup "value" doc
                return (TMValDouble v)
            String "Time" -> do
                v <- lookup "value" doc
                return (TMValTime v)
            String "String" -> do
                v <- lookup "value" doc
                return (TMValString (ST.fromText v))
            String "Octet" -> do
                v <- lookup "value" doc
                return (TMValOctet v)
            String "Nothing" -> do
                return TMValNothing
            _ -> Nothing
    cast' _ = Nothing

instance Val Validity where
    val x = Int32 (fromIntegral (getRawValidity x))

    cast' (Int32 x) = Just (Validity (fromIntegral x))
    cast' _         = Nothing


instance Val TMValue where
    val x =
        Doc
            [ "val" =: val (x ^. tmvalValue)
            , "validity" =: val (x ^. tmvalValidity)
            ]

    cast' (Doc doc) = do
        v  <- lookup "val" doc
        va <- lookup "validity" doc
        return (TMValue v va)
    cast' _ = Nothing


instance Val SPID where
    val (SPID x) = Int32 (fromIntegral x)

    cast' (Int32 x) = Just (SPID (fromIntegral x))
    cast' _         = Nothing


instance Val SleIf where
    val (SleRAFIf   x) = Doc [ "si" := String "RAF", "nr" := Int32 (fromIntegral x)]
    val (SleRCFIf   x) = Doc [ "si" := String "RCF", "nr" := Int32 (fromIntegral x)]
    val (SleFCLTUIf x) = Doc [ "si" := String "FCLTU", "nr" := Int32 (fromIntegral x)]
    val SleUnknownIf   = String "UNKNOWN"

    cast' (Doc doc) = do 
        si <- lookup "si" doc 
        nr <- lookup "nr" doc 
        case si of 
            String "RAF" -> Just (SleRAFIf nr) 
            String "RCF" -> Just (SleRCFIf nr) 
            String "FCLTU" -> Just (SleFCLTUIf nr) 
            _ -> Nothing 
    cast' _ = Nothing 