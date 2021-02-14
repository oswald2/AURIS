{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Mongo.Conversion.PUSPacket where


import           RIO                     hiding ( lookup )
--import qualified Data.ByteString               as B
--import qualified RIO.Text                      as T
import           Data.Bson

--import           General.Types
import           General.PUSTypes

import           Data.PUS.PUSPacket

import           Data.Mongo.Conversion.Class    ( MongoDbConversion(..) )
import           Data.Mongo.Conversion.Types    ( )
import           Data.Mongo.Conversion.PUSDfh   ( )


instance MongoDbConversion PUSHeader Document where
    toDB PUSHeader {..} =
        [ "version" =: _pusHdrTcVersion
        , "pktType" =: _pusHdrType
        , "dfh" =: _pusHdrDfhFlag
        , "apid" =: _pusHdrAPID
        , "segFlags" =: _pusHdrSeqFlags
        , "SSC" =: _pusHdrSSC
        , "length" =: _pusHdrTcLength
        ]
    fromDB doc = do
        version  <- lookup "version" doc
        pktType  <- lookup "pktType" doc
        dfh      <- lookup "dfh" doc
        apid     <- lookup "apid" doc
        segFlags <- lookup "segFlags" doc
        ssc      <- lookup "SSC" doc
        len      <- lookup "length" doc
        let pktID = PktID (packPktID version pktType dfh apid)
        return $ PUSHeader
            { _pusHdrPktID     = pktID
            , _pusHdrTcVersion = version
            , _pusHdrType      = pktType
            , _pusHdrDfhFlag   = dfh
            , _pusHdrAPID      = apid
            , _pusHdrSeqFlags  = segFlags
            , _pusHdrSSC       = ssc
            , _pusHdrSeqCtrl   = SeqControl (packSeqFlags segFlags ssc)
            , _pusHdrTcLength  = len
            }



instance MongoDbConversion PUSPacket Document where
    toDB PUSPacket {..} =
        [ "header" =: toDB _pusHdr
        , "dfh" =: toDB _pusDfh
        , "data" =: _pusData
        , "piVals" =: _pusPIs
        , "encodeCRC" =: _pusEncodeCRC
        ]
    fromDB doc = do
        hdr' <- lookup "header" doc
        hdr  <- fromDB hdr'
        dfh' <- lookup "dfh" doc
        dfh  <- fromDB dfh'
        d    <- lookup "data" doc
        piv  <- lookup "piVals" doc
        crc  <- lookup "encodeCRC" doc
        return $ PUSPacket { _pusHdr       = hdr
                           , _pusDfh       = dfh
                           , _pusPIs       = piv
                           , _pusData      = d
                           , _pusEncodeCRC = crc
                           }


instance Val TMPIVal where
    val TMPIVal {..} = Doc
        [ "value" =: _tmpiValue
        , "offset" =: _tmpiOffset
        , "tmpiWidth" =: _tmpiWidth
        ]

    cast' (Doc doc) = do
        v <- lookup "value" doc
        o <- lookup "offset" doc
        w <- lookup "tmpiWidth" doc
        return (TMPIVal v o w)
    cast' _ = Nothing


instance Val a => Val (a,a) where
    val (x, y) = Doc ["fst" =: val x, "snd" =: val y]

    cast' (Doc doc) = do
        f' <- lookup "fst" doc
        f  <- cast' f'
        --trace ("f: " <> T.pack (show f)) (return f)
        s' <- lookup "snd" doc
        s  <- cast' s'
        --trace ("s: " <> T.pack (show s)) (return s)
        return (f, s)
    cast' _ = Nothing


instance MongoDbConversion (Maybe (TMPIVal, TMPIVal)) Document where
    toDB Nothing  = ["TMPIVal" =: Null]
    toDB (Just v) = ["TMPIVal" =: v]

    fromDB doc = do
        t <- lookup "TMPIVal" doc
        case t of
            Null      -> Just Nothing
            d@(Doc _) -> cast' d
            _         -> Nothing

