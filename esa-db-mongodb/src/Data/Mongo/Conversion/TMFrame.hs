{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Mongo.Conversion.TMFrame where

import           RIO                     hiding ( lookup )

import           Data.Bson

import           General.Time
import           General.PUSTypes

import           Data.PUS.TMFrame
import           Data.PUS.TMStoreFrame


class MongoDbConversion a b | a -> b where 
  toDB :: a -> b 
  fromDB :: b -> Maybe a



instance MongoDbConversion TMStoreFrame Document where
    toDB TMStoreFrame {..} =
        [ "ert" =: timeToMicro _tmstTime
        , "frame" =: toDB _tmstFrame
        , "binary" =: Binary _tmstBinary
        ]
    fromDB doc = do 
      ert <- lookup "ert" doc 
      frame' <- lookup "frame" doc 
      frame <- fromDB frame'
      bin <- lookup "binary" doc 
      return $ TMStoreFrame (microToTime ert False) frame bin



instance MongoDbConversion TMFrame Document where
    toDB TMFrame {..} =
        [ "header" =: toDB _tmFrameHdr
        , "data" =: Binary _tmFrameData
        , "OCF" =: valMaybe ((fromIntegral <$> _tmFrameOCF) :: Maybe Int32)
        ]
    fromDB doc = do 
      hdr' <- lookup "header" doc 
      hdr <- fromDB hdr'
      d <- lookup "data" doc 
      ocf <- lookup "OCF" doc 
      return $ TMFrame hdr d ocf Nothing


instance MongoDbConversion TMFrameHeader Document where
    toDB TMFrameHeader {..} =
        [ "version" =: _tmFrameVersion
        , "SCID" =: _tmFrameScID
        , "VCID" =: _tmFrameVcID
        , "op_control" =: _tmFrameOpControl
        , "MCFC" =: _tmFrameMCFC
        , "VCFC" =: _tmFrameVCFC
        , "DFH" =: _tmFrameDfh
        , "sync" =: _tmFrameSync
        , "order" =: _tmFrameOrder
        , "seg_len" =: _tmFrameSegID
        , "fhp" =: _tmFrameFirstHeaderPtr
        ]
    fromDB doc = do
        v      <- lookup "version" doc
        scid   <- lookup "SCID" doc
        vcid   <- lookup "VCID" doc
        opctrl <- lookup "op_control" doc
        mcfc   <- lookup "MCFC" doc
        vcfc   <- lookup "VCFC" doc
        dfh    <- lookup "DFH" doc
        sync   <- lookup "sync" doc
        order  <- lookup "order" doc
        seglen <- lookup "seg_len" doc
        fhp    <- lookup "fhp" doc
        return $ TMFrameHeader v
                               scid
                               vcid
                               opctrl
                               mcfc
                               vcfc
                               dfh
                               sync
                               order
                               seglen
                               fhp


instance Val SCID where
    val (SCID x) = Int32 (fromIntegral x)

    cast' (Int32 x) = Just $ SCID (fromIntegral x)
    cast' _         = Nothing

instance Val VCID where
    val (VCID x) = Int32 (fromIntegral x)

    cast' (Int32 x) = Just $ VCID (fromIntegral x)
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
    cast' _ = Nothing

instance Val TMSegmentLen where
    val TMSegment256   = Int32 256
    val TMSegment512   = Int32 512
    val TMSegment1024  = Int32 1024
    val TMSegment65536 = Int32 65536

    cast' (Int32 256  ) = Just TMSegment256
    cast' (Int32 512  ) = Just TMSegment512
    cast' (Int32 1024 ) = Just TMSegment1024
    cast' (Int32 65536) = Just TMSegment65536
    cast' _             = Nothing
