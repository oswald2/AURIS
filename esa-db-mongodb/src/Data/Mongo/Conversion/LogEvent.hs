{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Mongo.Conversion.LogEvent where

import           RIO                     hiding ( lookup )
import qualified RIO.Text                      as T

import           Data.Bson
import           Data.Mongo.Conversion.Class

import           Persistence.LogEvent



instance MongoDbConversion LogEvent Document where
    toDB LogEvent {..} =
        [ "timestamp" =: UTC logEventTimestamp
        , "source" =: String logEventSource
        , "level" =: logEventLevel
        , "message" =: utf8BuilderToText logEventMessage
        ]

    fromDB doc = do
        t <- lookup "timestamp" doc
        s <- lookup "source" doc
        l <- lookup "level" doc
        m' <- lookup "message" doc
        m <- case m' of 
            String x -> Just (display x)
            _ -> Nothing 
        return $ LogEvent t s l m


instance Val LogLevel where
    val LevelDebug     = String "D"
    val LevelInfo      = String "I"
    val LevelWarn      = String "W"
    val LevelError     = String "E"
    val (LevelOther x) = String ("O_" <> textDisplay x)

    cast' (String "D") = Just LevelDebug
    cast' (String "I") = Just LevelInfo
    cast' (String "W") = Just LevelWarn
    cast' (String "E") = Just LevelError
    cast' (String x  ) = if "O_" `T.isPrefixOf` x
        then Just (LevelOther (T.drop 2 x))
        else Nothing
    cast' _ = Nothing
