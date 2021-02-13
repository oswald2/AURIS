{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Mongo.Conversion.LogEvent where

import           RIO

import           Data.Bson
import           Data.Mongo.Conversion.Class

import           Persistence.LogEvent



instance MongoDbConversion LogEvent Document where
    toDB _ = undefined

    fromDB _ = Nothing
