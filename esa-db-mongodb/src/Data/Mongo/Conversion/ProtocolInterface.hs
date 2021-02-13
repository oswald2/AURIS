{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Mongo.Conversion.ProtocolInterface where

import           RIO                     hiding ( lookup )

import           Data.Bson

import           Protocol.ProtocolInterfaces

import           Data.Mongo.Conversion.Class    ( MongoDbConversion(..) )
import           Data.Mongo.Conversion.Types    ( )



instance MongoDbConversion ProtocolInterface Document where
    toDB (IfNctrs x) = ["if" =: String "NCTRS", "nr" := val x]
    toDB (IfCnc   x) = ["if" =: String "CnC", "nr" := val x]
    toDB (IfEden  x) = ["if" =: String "EDEN", "nr" := val x]

    fromDB doc = do
        v <- lookup "if" doc
        n <- lookup "nr" doc
        case v of
            String "NCTRS" -> Just (IfNctrs n)
            String "CnC"   -> Just (IfNctrs n)
            String "EDEN"  -> Just (IfNctrs n)
            _              -> Nothing
