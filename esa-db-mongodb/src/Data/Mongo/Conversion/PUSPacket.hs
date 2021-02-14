{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Mongo.Conversion.PUSPacket where


import           RIO                     hiding ( lookup )

import           Data.Bson

import           General.Types
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
        return $ PUSHeader pktID
                           version
                           pktType
                           dfh
                           apid
                           segFlags
                           ssc
                           (SeqControl (packSeqFlags segFlags ssc))
                           len



instance MongoDbConversion PUSPacket Document where
    toDB PUSPacket {..} =
        [ "header" =: toDB _pusHdr
        , "dfh" =: toDB _pusDfh
        , "data" =: Binary (toBS _pusData)
        , "piVals" =: _pusPIs
        , "encodeCRC" =: _pusEncodeCRC
        ]
    fromDB _ = undefined


instance MongoDbConversion TMPIVal Document where
    toDB TMPIVal {..} =
        [ "value" =: _tmpiValue
        , "offset" =: _tmpiOffset
        , "tmpiWidth" =: _tmpiWidth
        ]

    fromDB doc = do
        v <- lookup "value" doc
        o <- lookup "offset" doc
        w <- lookup "tmpiWidth" doc
        return $ TMPIVal v o w

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


instance (MongoDbConversion a Document, Val a) => MongoDbConversion (a,a) Document where
    toDB (x, y) = ["fst" =: toDB x, "snd" =: toDB y]

    fromDB doc = do
        f <- lookup "fst" doc
        s <- lookup "s" doc
        return (f, s)

instance Val a => Val (a,a) where 
    val (x, y) = Doc ["fst" =: val x, "snd" =: val y]

    cast' (Doc doc) = do 
        f <- lookup "fst" doc
        s <- lookup "snd" doc 
        return (f, s)
    cast' _ = Nothing


instance MongoDbConversion (Maybe (TMPIVal, TMPIVal)) Document where
    toDB Nothing  = ["TMPIVal" =: String "Nothing"]
    toDB (Just v) = ["TMPIVal" =: v]

    fromDB doc = do 
        t <- lookup "TMPIVal" doc
        case t of 
            String "Nothing" -> Just Nothing 
            Doc doc2 -> fromDB doc2
            _ -> Nothing

