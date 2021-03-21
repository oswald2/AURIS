{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Mongo.Conversion.ProtocolInterface where

import           RIO                     hiding ( lookup )

import           Data.Bson

import           Protocol.ProtocolInterfaces

import           Data.Mongo.Conversion.Types    ( )



instance Val ProtocolInterface where
    val (IfNctrs x) = Doc ["if" =: String "NCTRS", "nr" := val x]
    val (IfCnc   x) = Doc ["if" =: String "CnC", "nr" := val x]
    val (IfEden  x) = Doc ["if" =: String "EDEN", "nr" := val x]

    cast' (Doc doc) = do
        v <- lookup "if" doc
        n <- lookup "nr" doc
        case v of
            String "NCTRS" -> Just (IfNctrs n)
            String "CnC"   -> Just (IfNctrs n)
            String "EDEN"  -> Just (IfNctrs n)
            _              -> Nothing
    cast' _ = Nothing
