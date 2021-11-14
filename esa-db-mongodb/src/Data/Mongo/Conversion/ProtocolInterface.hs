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
    val (IfSle   x) = Doc ["if" := String "SLE", "type" := val x]

    cast' (Doc doc) = do
        v <- lookup "if" doc
        case v of
            String "NCTRS" -> do 
                n <- lookup "nr" doc
                pure (IfNctrs n)
            String "CnC"   -> do 
                n <- lookup "nr" doc
                pure (IfCnc n)
            String "EDEN"  -> do 
                n <- lookup "nr" doc
                pure (IfEden n)
            String "SLE"   -> do 
                t <- lookup "type" doc
                pure (IfSle t)
            _              -> Nothing
    cast' _ = Nothing
