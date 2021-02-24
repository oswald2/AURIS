{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Mongo.Conversion.ExtractedDU where



import           RIO                     hiding ( lookup )

import           Data.Bson

import           General.Time

import           Data.PUS.ExtractedDU

import           Data.Mongo.Conversion.Class    ( MongoDbConversion(..) )
import           Data.Mongo.Conversion.Types    ( )
import           Data.Mongo.Conversion.ProtocolInterface
                                                ( )




instance MongoDbConversion (Maybe (Word32, Word32)) Document where
    toDB Nothing = ["gap" =: String "Nothing"]
    toDB (Just (low, high)) =
        [ "gap"
              =: [ "low" =: Int32 (fromIntegral low)
                 , "high" =: Int32 (fromIntegral high)
                 ]
        ]
    fromDB doc = do
        v <- lookup "gap" doc
        case v of
            String "Nothing" -> Nothing
            Doc    doc2      -> do
                low  <- lookup "low" doc2
                high <- lookup "high" doc2
                return (Just (low, high))
            _ -> Nothing


instance (MongoDbConversion a Document) => MongoDbConversion (ExtractedDU a) Document where
    toDB ExtractedDU {..} =
        [ "ert" =: timeToMicro _epERT
        , "quality" =: _epQuality
        , "gap" =: toDB _epGap
        , "source" =: _epSource
        , "vcid" =: _epVCID
        , "DU" =: toDB _epDU
        ]
    fromDB doc = do
        ert     <- lookup "ert" doc
        quality <- lookup "quality" doc
        gap'    <- lookup "gap" doc
        gap     <- fromDB gap'
        source  <- lookup "source" doc
        vcid    <- lookup "vcid" doc
        du'     <- lookup "DU" doc
        du      <- fromDB du'
        return $ ExtractedDU quality (microToTime ert False) gap source vcid du



