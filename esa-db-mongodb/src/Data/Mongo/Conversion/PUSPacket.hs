{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Mongo.Conversion.PUSPacket where


import           RIO                     hiding ( lookup )

import           Data.Bson

--import           General.Time
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
    toDB PUSPacket {..} = ["header" =: toDB _pusHdr, "dfh" =: toDB _pusDfh]
    fromDB _ = undefined
