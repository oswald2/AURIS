module Data.Mongo.Conversion.Class
    ( MongoDbConversion(..)
    ) where

import           RIO


class MongoDbConversion a b | a -> b where
  toDB :: a -> b
  fromDB :: b -> Maybe a

